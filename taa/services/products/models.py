
from taa import db
from taa.helpers import JsonSerializable

class ProductJsonSerializable(JsonSerializable):
    __json_hidden__ = ['cases']

class Product(ProductJsonSerializable, db.Model):
    __tablename__ = 'products'

    id = db.Column(db.Integer, primary_key=True)
    code = db.Column(db.String, nullable=False)
    name = db.Column(db.String, nullable=False)
    