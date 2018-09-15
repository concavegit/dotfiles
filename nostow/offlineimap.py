'''
gkgetsecret.py
This provides a handful of functions for retrieving secrets from GNOME Keyring
using the libsecret API. See the documentation for each function
'''

from gi import require_version
from gi.repository import Secret
require_version('Secret', '1')


def get_pw_from_desc(pw_desc):
    '''
    This function returns the password for an item in the default keyring
    which contains the description provided.
    Use this function if you created a password using the dialogue in Seahorse
    '''
    # Get service
    service = Secret.Service.get_sync(Secret.ServiceFlags.LOAD_COLLECTIONS)

    # Get default keyring
    keyring = Secret.Collection.for_alias_sync(service, "default",
                                               Secret.CollectionFlags.NONE, None)

    # Get keyring items
    items = keyring.get_items()

    # Load secrets
    Secret.Item.load_secrets_sync(items)

    # Loop through items, find the matching one and return its password
    password = None
    for item in items:
        if item.get_label() == pw_desc:
            password = item.get_secret().get_text()
            break

    # Close connection
    service.disconnect()

    return password
