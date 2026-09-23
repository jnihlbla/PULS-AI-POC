000100 01  W476FG1.                                                             
000200*                                 COPYTEXT TILL FARLIGT GODS WEB          
000300*                                 (HUVUD)                                 
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDFRASED             PIC X(15).                                   
000700*                                 FRAKTSEDELSNUMMER                       
000800     03 KVPAGE               PIC Z(4)9.                                   
000900*                                 ANTAL SIDOR                             
001000     03 IDORDNR7             PIC Z(7).                                    
001100*                                 ORDERNUMMER                             
001200     03 BEGMT-RAD1           PIC X(35).                                   
001300*                                 GODSMOTTAGARNAMN RAD 1                  
001400     03 BEGMT-RAD2           PIC X(35).                                   
001500*                                 GODSMOTTAGARNAMN RAD 2                  
001600     03 ADGMT-GATA           PIC X(35).                                   
001700*                                 GODSMOTTAGARADRESS GATA                 
001800     03 ADGMT-PADR           PIC X(35).                                   
001900*                                 GODSMOTTAGARADRESS POSTADRESS           
002000     03 ADGMT-LAND           PIC X(35).                                   
002100*                                 GODSMOTTAGARADRESS LAND                 
002200     03 AIRPORT              PIC X(12).                                   
002300     03 SIGN-ORT             PIC X(20).                                   
002400     03 SIGN-DATUM           PIC 9(6).                                    
002500     03 KDFORMS              PIC X.                                       
002600*                                 KOD FÖR FORMS-TYP                       
002700     03 DCS-BEGMT-RAD1       PIC X(35).                                   
002800*                                 GODSMOTTAGARNAMN RAD 1                  
002900     03 DCS-BEGMT-RAD2       PIC X(35).                                   
003000*                                 GODSMOTTAGARNAMN RAD 2                  
003100*** END OF VILMAII-COPY LENGTH= 321 BYTES                                 
