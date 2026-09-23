000100 01  RESP-WL0107O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0107         
000300*                                 LDC GOODS RECEIVING HISTORY             
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-IDARTNR-KEY     PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 RESP-BEART           PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200*                                 PART DESCRIPTION                        
001300     03 RESP-KVRADER         PIC Z(4)9.                                   
001400*                                 ANTAL RADER                             
001500*                                 NUMBER OF LINES                         
001600     03 RESP-FLTRACK         PIC X.                                       
001700*                                 FLAG FOR TRACKING-ID FOR A DC           
001800*                                                                         
001900*                                 FLAG FOR TRACKING-ID FOR A DC           
002000*                                                                         
002100     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002200*                                 GRUPP MED TABELLRADER                   
002300        05 RESP-IDPTYP       PIC X(3).                                    
002400*                                 POSTTYP                                 
002500*                                 RECORD TYPE                             
002600        05 RESP-IDDC         PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800*                                 WAREHOUSE IDENTIFIER                    
002900        05 RESP-IDLEVNR      PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003200        05 RESP-KDRT         PIC Z9.                                      
003300*                                 REDOVISNINGSTYP                         
003400*                                 TYPE OF ACCOUNTING                      
003500        05 RESP-IDLOPNRM     PIC Z(7)9.                                   
003600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
003700*                                 (0VVDLLLLK)                             
003800*                                 SERIAL NO RECEIVING REPORT              
003900*                                 (0WWDLLLLC)                             
004000        05 RESP-IDKUNDRF     PIC X(10).                                   
004100*                                 KUNDENS REFERENS (ORDERID)              
004200*                                 CUSTOMER REFERENCE (ORDER ID)           
004300        05 RESP-TIREGDAT     PIC 9(6).                                    
004400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004500*                                 REGISTRATION DATE (YYMMDD)              
004600        05 RESP-TIINLINL     PIC 9(6).                                    
004700*                                 RAPPORTERINGSDATUM INLAGD (R32)         
004800*                                 DATE OF REPORTED IN STOCK (R32)         
004900        05 RESP-KVAVIS       PIC -(6)9.                                   
005000*                                 AVISERAT ANTAL                          
005100*                                 QUANTITY NOTIFIED                       
005200        05 RESP-KVANTMOT     PIC -(6)9.                                   
005300*                                 ANTAL MOTTAGET                          
005400*                                 QUANTITY RECEIVED                       
005500        05 RESP-KVART-SKROT  PIC Z(6)9.                                   
005600*                                 ANTAL SKROTADE ARTIKLAR                 
005700*                                 QUANTITY INSPECTED PARTS                
005800        05 RESP-FLMAKUL      PIC X.                                       
005900*                                 FLAGGA MAKULERAT KOLLI                  
006000*                                 CASE CANCELLATION FLAG                  
006100        05 RESP-IDKUNDNR     PIC Z(7).                                    
006200*                                 KUNDNUMMER                              
006300*                                 CUSTOMER NO                             
006400        05 RESP-IDKOLLI      PIC Z(4)9.                                   
006500*                                 KOLLINUMMER                             
006600*                                 CASE NUMBER                             
006700        05 RESP-IDTRACK      PIC X(25).                                   
006800*                                 TRACKING ID FROM CUSTOMS                
006900*                                 CUSTOMS TRACKING ID                     
007000        05 RESP-KVRETUR      PIC -(6)9.                                   
007100*                                 ANTAL I RETUR                           
007200*                                 QUANTITY IN RETURN                      
007300        05 RESP-KDAVVANT     PIC 9.                                       
007400*                                 AVVIKELSEANTAL KOD                      
007500*                                 0=INGEN ANM.   1=AVVIKELSE              
007600*                                 2=MAKULERING AV MOTT.RAPPORT            
007700*                                 QUANTITY DEVIATION  CODE                
007800*                                 0=NO DEV.    1=DEVIATION                
007900*                                 2=CANCELLING OF REC. REPORT             
008000        05 RESP-IDUSER-003   PIC X(5).                                    
008100*                                 ANSVARIGT USERID INLÄGGN.(R32)          
008200        05 RESP-FLTULLST     PIC X.                                       
008300*                                 FLAGGA FÖR ART STOPPAD I TULLEN         
008400*                                 FLAG FOR PART HOLD AT CUSTOM            
008500        05 RESP-KVTULRET     PIC Z(6)9.                                   
008600*                                 ANTAL SOM TULLEN RETURNERAT             
008700*                                 RETURNED QUANTITY FROM CUSTOM           
008800*** END OF VILMAII-COPY LENGTH= 61042 BYTES                               
