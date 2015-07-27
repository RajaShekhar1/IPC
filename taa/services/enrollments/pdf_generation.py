from PyPDF2 import PdfFileReader, PdfFileWriter
from io import BytesIO


def merge_pdfs(base_bytes, overlay_bytes, outpath=None):
    base_reader = PdfFileReader(base_bytes)
    overlay_reader = PdfFileReader(overlay_bytes)
    writer = PdfFileWriter()

    for page in range(base_reader.getNumPages()):
        if page >= overlay_reader.getNumPages():
            break

        base_page = base_reader.getPage(page)
        overlay_page = overlay_reader.getPage(page)
        base_page.mergePage(overlay_page)
        writer.insertPage(base_page, page)

    if outpath is None:
        buf = BytesIO()
        writer.write(buf)
        return buf.getvalue()
    else:
        with open(outpath, 'wb') as f:
            writer.write(f)
