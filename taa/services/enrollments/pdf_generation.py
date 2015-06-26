from PyPDF2 import PdfFileReader, PdfFileWriter
import StringIO

class PdfOverlay:
    def mergepdfs(self, pdf1, pdf2, saveTo):
        bytes1 = StringIO.StringIO(open(pdf1, 'rb').read())
        overlay = PdfFileReader(bytes1)
        source = PdfFileReader(pdf2)
        writer = PdfFileWriter()

        sourcePage = source.getPage(0)
        overlayPage = overlay.getPage(0)

        sourcePage.mergePage(overlayPage)

        writer.insertPage(sourcePage, 0)

        with open(saveTo, 'wb') as fp:
            writer.write(fp)
