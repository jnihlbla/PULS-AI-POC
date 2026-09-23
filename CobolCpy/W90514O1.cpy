000100 01  RESP-W90514O1.                                                       
000200*                                 RESPONSE FROM PGM W90514                
000300*                                                                         
000400     03 RESP-WZ01RES2.                                                    
000500*                                 THE FIRST FIELDS IN AN RESPONSE         
000600*                                 SENT AS A RESULT OF A REQUEST           
000700*                                 FROM ONE SYSTEM COMPONENT TO            
000800*                                 ANOTHER.                                
000900*                                 !!! SECOND VERSION / IDRESVER =         
001000*                                  002 !!!                                
001100        05 RESP-IDRESVER     PIC 9(3).                                    
001200*                                 VERSIONSNUMMER FÖR MEDDELANDE O         
001300*                                 M SVARSHUVUD                            
001400        05 RESP-IDMSG-INFO   PIC X(3).                                    
001500*                                 INFORMATIONSMEDDELANDE ID               
001600        05 RESP-IDMSG-ERROR  PIC X(3).                                    
001700*                                 FELMEDDELANDE ID                        
001800        05 RESP-IDELMT-ERROR PIC X(16).                                   
001900*                                 DATAELEMENTIDENTITET                    
002000        05 RESP-KDSTATUS-API PIC 9(3).                                    
002100        05 RESP-MESSAGES     OCCURS 2 TIMES.                              
002200*                                                                         
002300           07 RESP-IDMSG     PIC X(10).                                   
002400*                                 MEDDELANDE NUMMER                       
002500           07 RESP-MESSAGE   PIC X(100).                                  
002600*                                 MEDDELANDE                              
002700        05 FILLER            PIC X(220).                                  
002800     03 RESP-IDARTNR-UT      PIC 9(9).                                    
002900*                                 ARTIKELNUMMER                           
003000     03 RESP-W200CDCI.                                                    
003100*                                 DATA FROM SUB PROGRAM W200CDCI          
003200        05 RESP-BEFT         PIC 9(3).                                    
003300*                                 FÖRPACKNINGSTYP                         
003400        05 RESP-IDARTNR-EMBQ0                                             
003500                             PIC 9(9).                                    
003600*                                 EMBALLAGEARTIKELNR FÖR Q0               
003700        05 RESP-IDARTNR-EMBQ1                                             
003800                             PIC 9(9).                                    
003900*                                 EMBALLAGEARTIKELNR FÖR Q1               
004000        05 RESP-IDARTNR-EMBQ2                                             
004100                             PIC 9(9).                                    
004200*                                 EMBALLAGEARTIKELNR FÖR Q2               
004300        05 RESP-IDARTNR-EMBQ3                                             
004400                             PIC 9(9).                                    
004500*                                 EMBALLAGEARTIKELNR FÖR Q3               
004600        05 RESP-IDPSN        PIC 9(3).                                    
004700*                                 PROPER SHIPPING NAME                    
004800        05 RESP-KVQPACK-3    PIC 9(5).                                    
004900*                                 ANTAL I Q3 FÖRPACKNING                  
005000        05 RESP-KVAVROP      PIC 9(7).                                    
005100*                                 AVROPSKVANTITET                         
005200        05 RESP-TIAVROP-AVS  PIC 9(4).                                    
005300*                                 AVSÄNDNINGSVECKA (PLANERAD)             
005400*                                 (ÅÅVV)                                  
005500        05 RESP-VKART        PIC 9(7).                                    
005600*                                 ARTIKELVIKT (G)                         
005700        05 RESP-VLARTNTO     PIC 9(8).                                    
005800*                                 ARTIKELVOLYM (CM3)                      
005900        05 RESP-ADLAGOMR     PIC 9(2).                                    
006000*                                 LAGEROMRÅDE                             
006100        05 RESP-ADGANG       PIC 9(2).                                    
006200*                                 GÅNG                                    
006300        05 RESP-ADPLATS      PIC 9(5).                                    
006400*                                 LAGERPLATSNUMMER                        
006500        05 RESP-KDARTURS     PIC X(2).                                    
006600*                                 ARTIKELURSPRUNGSKOD                     
006700        05 RESP-IDLEVNR-MFG  PIC X(5).                                    
006800*                                 LEVERANTÖRNUMMER                        
006900        05 RESP-IDLEVNR-SHIP PIC X(5).                                    
007000*                                 SKEPPANDE LEVERANTÖR                    
007100        05 RESP-KVVECKOR-LT  PIC 9(3).                                    
007200*                                 ANTAL VECKOR LEDTID                     
007300        05 RESP-KVDAGAR-TT   PIC 9(3).                                    
007400*                                 DAGAR TULL- OCH TRANSPORT-TID           
007500        05 RESP-KVDAGAR-INLEV                                             
007600                             PIC 9(3).                                    
007700*                                 INLEVERANSTID     (ANTAL DAGAR)         
007800        05 RESP-TIETA        PIC 9(5).                                    
007900*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
008000        05 RESP-KDFARLIG     PIC 9.                                       
008100*                                 KOD FÖR FARLIGT GODS                    
008200*** END OF VILMAII-COPY LENGTH= 586 BYTES                                 
