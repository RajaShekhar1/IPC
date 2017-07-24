import json
from okta.framework.ApiClient import ApiClient
from okta.framework.Utils import Utils
from okta.models.usergroup.UserGroup import UserGroup
from okta.models.user.User import User

class RelatedResourcesClient(ApiClient):
    def __init__(self, base_url, api_token):
        ApiClient.__init__(self, base_url + '/api/v1/', api_token)

    # CRUD

    def get_user_in_groups(self, uid):
        """Get a list of UserGroups

        :param limit: maximum number of groups to return
        :type limit: int or None
        :param query: string to search group names
        :type query: str or None
        :rtype: list of UserGroup
        """
        response = ApiClient.get_path(self, 'users/{0}/groups'.format(uid))
        groups=[]
        return Utils.deserialize(response.text,UserGroup)

    def get_user(self, uid):
        """Get a list of UserGroups

        :param limit: maximum number of groups to return
        :type limit: int or None
        :param query: string to search group names
        :type query: str or None
        :rtype: list of UserGroup
        """
        response = ApiClient.get_path(self, 'users/{0}'.format(uid))
        return json.loads(response.text)

    def add_group_with_user(self, gid, uid):
        return ApiClient.put_path(self, 'groups/{0}/users/{1}'.format(gid,uid))

    def delete_group_with_user(self, gid, uid):
        return ApiClient.delete_path(self, 'groups/{0}/users/{1}'.format(gid,uid))

    def update_user_profile(self, uid, data):
        return ApiClient.post_path(self, 'users/{0}'.format(uid) , data )

    def add_user(self, data):
        response =  ApiClient.post_path(self, 'users?activate=true', data )
        return Utils.deserialize(response.text, User)


    def delete_user(self, uid):
        return  ApiClient.delete_path(self, 'users/{0}'.format(uid) )


