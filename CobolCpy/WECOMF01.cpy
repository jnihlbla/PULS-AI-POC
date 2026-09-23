001000*** EDIT ALLOWED                                                          
010001 01  WECOMF01.                                                            
020010*                                 ECOM MESSAGE REQUEST BODY               
022000*                                                                         
024010     03 ECOM-HDR-KDVALISO-LTH         PIC S9999 COMP-5 SYNC.              
025010     03 ECOM-HDR-KDVALISO             PIC X(3).                           
025020     03 ECOM-HDR-PRKURS               PIC S9(6)V9(5) COMP-3.              
026010     03 ECOM-HDR-DAFINDOC-LTH         PIC S9999 COMP-5 SYNC.              
027010     03 ECOM-HDR-DAFINDOC             PIC X(8).                           
028009*                                                                         
029010     03 ECOM-LINE-ITEMS-NUM           PIC S9(9) COMP-5 SYNC.              
029109     03 ECOM-LINE-ITEMS OCCURS 999 TIMES.                                 
029110       05 ECOM-LINE-IDARTNR-LTH       PIC S9999 COMP-5 SYNC.              
029210       05 ECOM-LINE-IDARTNR           PIC X(9).                           
029310       05 ECOM-LINE-IDEXCUST-2-LTH    PIC S9999 COMP-5 SYNC.              
029410       05 ECOM-LINE-IDEXCUST-2        PIC X(15).                          
029510       05 ECOM-LINE-BEART-LTH         PIC S9999 COMP-5 SYNC.              
029610       05 ECOM-LINE-BEART             PIC X(25).                          
029710       05 ECOM-LINE-REARTRAB          PIC S9(2)V9(2) COMP-3.              
030010       05 ECOM-LINE-PRARTNTO          PIC S9(7)V9(2) COMP-3.              
050010       05 ECOM-LINE-SUNTO             PIC S9(7)V9(2) COMP-3.              
050020       05 ECOM-LINE-KDANMORS-NUM      PIC S9(9) COMP-5 SYNC.              
050030       05 ECOM-LINE-KDANMORS-LVL.                                         
050110          07 ECOM-LINE-KDANMORS-LTH   PIC S9999 COMP-5 SYNC.              
050111          07 ECOM-LINE-KDANMORS       PIC X(2).                           
050112       05 ECOM-LINE-IDDC-LTH          PIC S9999 COMP-5 SYNC.              
050113       05 ECOM-LINE-IDDC              PIC X(2).                           
050114       05 ECOM-LINE-DAFAKREF-NUM      PIC S9(9) COMP-5 SYNC.              
050115       05 ECOM-LINE-DAFAKREF-LVL.                                         
050116          07 ECOM-LINE-DAFAKREF-LTH   PIC S9999 COMP-5 SYNC.              
050117          07 ECOM-LINE-DAFAKREF       PIC X(8).                           
050118       05 ECOM-LINE-IDFAKREF-NUM      PIC S9(9) COMP-5 SYNC.              
050119       05 ECOM-LINE-IDFAKREF-LVL.                                         
050120          07 ECOM-LINE-IDFAKREF-LTH   PIC S9999 COMP-5 SYNC.              
050130          07 ECOM-LINE-IDFAKREF       PIC X(9).                           
051010       05 ECOM-LINE-PRARTBTO          PIC S9(7)V9(2) COMP-3.              
051110       05 ECOM-LINE-IDREF-LTH         PIC S9999 COMP-5 SYNC.              
051210       05 ECOM-LINE-IDREF             PIC X(15).                          
051310       05 ECOM-LINE-BEVOLREF-LTH      PIC S9999 COMP-5 SYNC.              
051410       05 ECOM-LINE-BEVOLREF          PIC X(10).                          
051420       05 ECOM-LINE-PARCEL-ID-LTH     PIC S9999 COMP-5 SYNC.              
051430       05 ECOM-LINE-PARCEL-ID         PIC X(22).                          
051510       05 ECOM-LINE-KVLEVART          PIC S9(9) COMP-5 SYNC.              
051610       05 ECOM-LINE-UOM-LTH           PIC S9999 COMP-5 SYNC.              
051710       05 ECOM-LINE-UOM               PIC X(6).                           
051810*                               PIECES                                    
051910       05 ECOM-LINE-REVAT             PIC S9(3)V9(2) COMP-3.              
052010       05 FILLER                      PIC X(1).                           
052110     03 ECOM-HDR-IDFINDOC-LTH         PIC S9999 COMP-5 SYNC.              
052810     03 ECOM-HDR-IDFINDOC             PIC X(9).                           
052910     03 ECOM-HDR-KDFINDOC-LTH         PIC S9999 COMP-5 SYNC.              
053010     03 ECOM-HDR-KDFINDOC             PIC X(7).                           
053110*                               INVOICE / CREDIT                          
053309     03 ECOM-HDR-IDPARTNR-FROM-LTH    PIC S9999 COMP-5 SYNC.              
053409     03 ECOM-HDR-IDPARTNR-FROM        PIC X(9).                           
053510     03 ECOM-HDR-IDPARTNR-TO-LTH      PIC S9999 COMP-5 SYNC.              
053609     03 ECOM-HDR-IDPARTNR-TO          PIC X(9).                           
053810     03 ECOM-FOOT-SUBTO-TOT           PIC S9(11)V9(2) COMP-3.             
053910     03 ECOM-FOOT-SUNTO-TOT           PIC S9(11)V9(2) COMP-3.             
054010     03 ECOM-FOOT-SUVAT-BILLIT-TOT    PIC S9(11)V9(2) COMP-3.             
054210     03 ECOM-HDR-IDVAT-RESP-LTH       PIC S9999 COMP-5 SYNC.              
054310     03 ECOM-HDR-IDVAT-RESP           PIC X(17).                          
054410     03 ECOM-HDR-IDVAT-BET-LTH        PIC S9999 COMP-5 SYNC.              
054510     03 ECOM-HDR-IDVAT-BET            PIC X(17).                          
055000*** END OF VILMAII-COPY LENGTH=                                           
