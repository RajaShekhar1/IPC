import base64
import glob
import os
import re
import sys


PDF_TAG = 'AttachmentData'
RE_PDF = re.compile('<{tag}>(.*?)</{tag}>'.format(tag=PDF_TAG))


def extract_raw(path):
    with open(path, 'r') as f:
        d = f.read()
    RE_PDF.search(d)
    m = RE_PDF.search(d)
    if m is None:
        return None
    return m.group(1)


def extract_pdf(pathstr):
    for path in glob.glob(pathstr):
        encoded = extract_raw(path)
        if encoded is None:
            print("'{}': No PDF found".format(path))
        else:
            outpath = ''.join([os.path.splitext(os.path.basename(path))[0],
                               os.extsep, 'pdf'])
            with open(outpath, 'wb') as o:
                o.write(base64.b64decode(encoded))
            print("'{}': Wrote {:,} bytes to '{}'".format(
                    path, os.path.getsize(outpath), outpath))


def usage(args):
    print(
        "Extract base64-encoded PDF data from enrollment XML.\n"
        "\n"
        "Usage:\n"
        "\n"
        "    {} PATH1 [PATH2 .. PATHn]\n"
        "\n"
        " PATH - Path to XML file(s) containing encoded PDF data. "
        "Surround wildcards with 'single quotes' to prevent shell wildcard expansion\n"
        "\n".format(args[0]))


if __name__ == '__main__':
    if len(sys.argv) == 2:
        extract_pdf(sys.argv[1])
    else:
        usage(sys.argv)
