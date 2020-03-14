#!/usr/bin/env python3
import os
import re
import urllib
import urllib.request
import urllib.parse


def parse_elisp_recipe(filepath):
    """
    Take an elisp file and extract it's recipe and return it's lines.
    """
    with open(filepath, 'rb') as fh:
        data = fh.read()

    consuming = False
    recipe = []
    lines = data.splitlines()
    for i, line in enumerate(lines, 1):
        line = line.strip()
        if line == b';;; Recipe:':
            consuming = True
        elif consuming:
            if line:
                if not line.startswith(b';; '):
                    print(f'Expected ";; " prefix at: {filepath:s}:{i:d}')
                    return False
                recipe.append(line[3:])
            else:
                # Blank line after having read a recipe,
                # finish reading the block.
                if recipe:
                    break
        else:
            # Not yet consuming.
            pass

    try:
        recipe = eval(b'\n'.join(recipe))
    except Exception as ex:
        print(f'Error parsing {filepath:s} {ex}')
        recipe = None

    return recipe, lines


def code_block_from_stackexchange_url(
        url,
        code_block_index,
        *,
        use_cache=False,
):
    # This function could be made a lot cleaner,
    # it happens to work for for now.
    # If we want to be clever this should use a proper web-browser
    # and scrape the content.

    # Only for cache.
    response = None
    if use_cache:
        url_encode = 'cache_' + urllib.parse.quote(url, safe='') + '.html'

        if not os.path.exists(url_encode):
            response = urllib.request.urlopen(url)
            data = response.read()
            with open(url_encode, 'wb') as fh:
                fh.write(data)
        else:
            with open(url_encode, 'rb') as fh:
                data = fh.read()
    else:
        response = urllib.request.urlopen(url)
        data = response.read()
    del response

    answer_id = url.rstrip('/').rsplit('/', 1)[-1]

    anchor = b'<a name="' + answer_id.encode('utf-8') + b'">'
    if anchor not in data:
        print(f'URL misses {anchor!r}')
        return None

    data_strip = data.split(anchor, 1)[1]

    code_blocks = []

    # Get the results between.
    # <pre class="lang-el prettyprint-override"><code>
    # ...
    # </code></pre>
    code_block_begin = b'<pre class="lang-el prettyprint-override"><code>'
    code_block_end = b'</code></pre>'
    for block in data_strip.split(code_block_begin)[1:]:
        block, _ = block.split(code_block_end, 1)
        code_blocks.append(block)

    # TODO, error check this...
    code = code_blocks[code_block_index]
    code = code.decode('utf-8')
    return code


def update_from_stackexchange(filepath, *, use_cache=False):
    filename = os.path.basename(filepath)
    filename_no_ext = os.path.splitext(filename)[0]

    recipe, lines = parse_elisp_recipe(filepath)

    if not recipe:
        # Error already reported.
        return None

    file_head = []
    file_head_found_code = False

    for line in lines:
        file_head.append(line)
        if line == b';;; Code:':
            file_head_found_code = True
            break
    if not file_head_found_code:
        print(
            f'Error parsing {filepath:s}, '
            'no ";;; Code:" preceding the code.'
        )
        return False
    del file_head_found_code

    file_head = b'\n'.join(file_head).decode('utf-8')

    url = recipe['url']

    code = code_block_from_stackexchange_url(
        recipe['url'],
        recipe.get('index', 0),
        use_cache=use_cache,
    )

    if not code:
        # Error must be printed already.
        return False

    # We could add more, add as needed.
    code = code.replace(
        '&lt;', '<',
    ).replace(
        '&gt;', '>',
    ).replace(
        '&amp;', '&',
    )

    # Run replacements:
    for src, dst in recipe.get('replace', ()):
        code = re.sub(src, dst, code)

    with open(filepath, 'w', encoding='utf-8') as fh:
        fh.write(file_head)
        fh.write('\n\n')
        fh.write(code.rstrip())

        fh.write(
            '\n\n'
            f'(provide \'{filename_no_ext})\n'
            f';;; {filename} ends here\n'
        )

    return True


def update_readme_from_files(root, files, readme_file):
    readme_lines = []

    for i, f in enumerate(files, 1):
        print(
            f'Updating {f!r} '
            f'{i} of {len(files)}'
        )

        filepath = os.path.join(root, f)
        recipe, lines = parse_elisp_recipe(filepath)

        filename_no_ext = os.path.splitext(f)[0]
        # TODO: could use a regex for this.
        terse_description = (
            lines[0].decode('utf-8').split(' --- ', 1)[1].split(' -*- ', 1)[0]
        )

        if recipe is None:
            continue

        url = recipe['url']
        readme_lines += [
            f'`{filename_no_ext} <{url}>`__',
            f'   {terse_description}.',
        ]

    readme_filepath = os.path.join(root, readme_file)
    with open(readme_filepath, 'r', encoding='utf-8') as fh:
        data = fh.read()

    help_begin_text = '.. BEGIN PACKAGE LIST'
    help_end_text = '.. END PACKAGE LIST'
    help_begin_index = data.find(help_begin_text)
    help_end_index = data.find(help_end_text, help_begin_index)

    if help_begin_index == -1:
        print('Error: {!r} not found'.format(help_begin_text))
        return
    if help_end_index == -1:
        print('Error: {!r} not found'.format(help_end_text))
        return

    help_begin_index += len(help_begin_text) + 1

    data_update = (
        data[:help_begin_index] + '\n' +
        '\n'.join(readme_lines) + '\n\n' +
        data[help_end_index:]
    )

    with open(readme_filepath, 'w', encoding='utf-8') as f:
        f.write(data_update)


def argparse_create():
    import argparse

    parser = argparse.ArgumentParser(
        description='Update elisp scripts from stackexchange',
    )

    parser.add_argument(
        "-C", "--use-cache",
        dest='use_cache',
        default=False,
        action='store_true',
        help=(
            'Download the web-sites into the current directory for re-use '
            '(useful to quickly test updates).'
        ),
        required=False,
    )

    parser.add_argument(
        "-R", "--update-readme",
        dest='use_update_readme',
        default=False,
        action='store_true',
        help=(
            'Update the package list in "readme.rst".'
        ),
        required=False,
    )

    return parser


def main():
    root = os.path.normpath(os.path.join(os.path.dirname(__file__), os.pardir))

    parser = argparse_create()
    args = parser.parse_args()

    files = [
        f for f in
        os.listdir(root)
        if f.endswith('.el') and (not f.startswith('.'))
    ]
    files.sort()

    for i, f in enumerate(files, 1):
        print(
            f'Updating {f!r} '
            f'{i} of {len(files)}'
        )
        update_from_stackexchange(
            os.path.join(root, f),
            use_cache=args.use_cache,
        )

    if args.use_update_readme:
        update_readme_from_files(root, files, "readme.rst")


if __name__ == '__main__':
    main()
