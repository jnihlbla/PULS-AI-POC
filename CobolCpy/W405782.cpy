000100 01  W405782.                                                             
000200*                                 COPYTEXT TILL DOCUMENT LINE BO-         
000300*                                 RELEASE                                 
000400     03 LINE-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 LINE-KDPGM           PIC X.                                       
000700     03 LINE-IDDISTR         PIC Z(3)9.                                   
000800*                                 DISTRIKTNUMMER                          
000900     03 LINE-IDKUNDNR        PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100     03 LINE-IDARTNR         PIC Z(7)9.                                   
001200*                                 ARTIKELNUMMER                           
001300     03 LINE-BEART-ENG       PIC X(25).                                   
001400*                                 ENGELSK ARTIKELBENÄMNING                
001500     03 LINE-KVART           PIC Z(6)9.                                   
001600*                                 ANTAL ARTNR PER BRYTBEGREPP             
001700     03 LINE-IDKUNDRF        PIC X(10).                                   
001800*                                 KUNDENS REFERENS (ORDERID)              
001900     03 LINE-TIREPDAT        PIC X(6).                                    
002000*                                 REPAIR DATE                             
002100     03 LINE-IDKUNDRF-WIP    PIC X(10).                                   
002200*                                 REPARATIONS ORDERNR, LDC KUND           
002300*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
