#include <webkit2/webkit2.h>
#include <gtk/gtk.h>

int main(void)
{
    GtkApplication *a = gtk_application_new("org.org", 0);
    WebKitSettings *wks = webkit_settings_new();
    WebKitWebView *wv = webkit_web_view_new_with_settings(wks);
    return 0;
}
