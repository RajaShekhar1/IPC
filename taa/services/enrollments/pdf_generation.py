from PyPDF2 import PdfFileReader, PdfFileWriter


def merge_pdfs(base_bytes, overlay_bytes, outpath):
    base_reader = PdfFileReader(base_bytes)
    overlay_reader = PdfFileReader(overlay_bytes)
    writer = PdfFileWriter()

    for page in range(base_reader.getNumPages()):
        base_page = base_reader.getPage(page)
        overlay_page = overlay_reader.getPage(page)
        base_page.mergePage(overlay_page)
        writer.insertPage(base_page, page)

    with open(outpath, 'wb') as f:
        writer.write(f)
