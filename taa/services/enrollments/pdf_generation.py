from PyPDF2 import PdfFileReader, PdfFileWriter
import StringIO

def merge_pdfs(pdf1, pdf2, saveTo):
    bytes1 = StringIO.StringIO(open(pdf1, 'rb').read())
    overlay = PdfFileReader(bytes1)
    source = PdfFileReader(pdf2)
    writer = PdfFileWriter()

    for page in range(source.getNumPages()):
        sourcePage = source.getPage(page)
        overlayPage = overlay.getPage(page)
        sourcePage.mergePage(overlayPage)
        writer.insertPage(sourcePage, page)

    with open(saveTo, 'wb') as fp:
        writer.write(fp)
