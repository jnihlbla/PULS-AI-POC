000100 01  RESP-W60109O1.                                                       
000200*                                 RESPCOPYTEXT TILL W601109               
000300     03 RESP-IDARTNR         PIC Z(7)9.                                   
000400*                                 ARTIKELNUMMER                           
000500     03 RESP-KVAVIS-HUV      PIC Z(5)9.                                   
000600*                                 AVISERAT ANTAL                          
000700     03 RESP-BEART           PIC X(25).                                   
000800*                                 ARTIKELBENÄMNING                        
000900     03 RESP-KDKVAINL        PIC X(2).                                    
001000*                                 INLEVERANS TILL KVALITETSKOLL           
001100     03 RESP-IDFS            PIC X(8).                                    
001200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001300     03 RESP-TIAVIDAT        PIC 9(6).                                    
001400*                                 AVISERINGSDATUM (YYMMDD)                
001500     03 RESP-IDLBBET         PIC X(12).                                   
001600*                                 LASTBÄRARBETECKNING                     
001700     03 RESP-KDRT            PIC Z9.                                      
001800*                                 REDOVISNINGSTYP                         
001900     03 RESP-FLKVAANT        PIC X.                                       
002000*                                 ANTALSKONTROLL UTFÖRD                   
002100     03 RESP-IDLEVNR         PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 RESP-TIINLMOT        PIC 9(6).                                    
002400*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
002500     03 RESP-KVAVIS          PIC Z(5)9.                                   
002600*                                 AVISERAT ANTAL                          
002700     03 RESP-KDSORT-VIKT     PIC X(2).                                    
002800*                                 SORT-KOD                                
002900     03 RESP-VKART           PIC Z(6)9.                                   
003000*                                 ARTIKELVIKT (G)                         
003100     03 RESP-ADLAGOMR        PIC Z9.                                      
003200*                                 LAGEROMRÅDE                             
003300     03 RESP-ADGANG          PIC Z9.                                      
003400*                                 GÅNG                                    
003500     03 RESP-ADPLATS         PIC Z(4)9.                                   
003600*                                 LAGERPLATSNUMMER                        
003700     03 RESP-KVAVIS-PRIO     PIC Z(5)9.                                   
003800*                                 BERÄKN PRIORITERAD KVANT TOT            
003900     03 RESP-BESORT-VOLYM    PIC X(6).                                    
004000*                                 BENÄMNING PÅ SORT/ENHET                 
004100     03 RESP-VLARTNTO        PIC Z(7)9.9.                                 
004200*                                 ARTIKELVOLYM NETTO (CM3)                
004300     03 RESP-ADBUFFOMR       PIC Z9.                                      
004400*                                 BUFFERTOMRÅDE                           
004500     03 RESP-ADBUFFGANG      PIC Z9.                                      
004600*                                 BUFFERT GÅNG                            
004700     03 RESP-ADBUFFPL        PIC Z(4)9.                                   
004800*                                 BUFFERPLATSNUMMER                       
004900     03 RESP-KVKVAPRIM       PIC Z(5)9.                                   
005000*                                 ANTAL TILL PRIMÄRKONTROLL               
005100     03 RESP-KDLAGEMB        PIC X(4).                                    
005200*                                 EMBALLAGEBETECKNING                     
005300     03 RESP-KDARTURS        PIC X(2).                                    
005400*                                 ARTIKELURSPRUNGSKOD                     
005500     03 RESP-KVKVASEK        PIC Z(5)9.                                   
005600*                                  ANTAL TILL SEKUNDÄRKONTROLL            
005700     03 RESP-BEFT            PIC Z9.                                      
005800*                                 FÖRPACKNINGSTYP                         
005900     03 RESP-IDANSK          PIC Z(2)9.                                   
006000*                                 ANSKAFFARNUMMER                         
006100     03 RESP-ADTRDEST        PIC X(3).                                    
006200*                                 TRANSPORTDESTINATION                    
006300     03 RESP-KVAVIS-KIT      PIC Z(5)9.                                   
006400*                                 AVISERAT ANTAL FÖR SATS                 
006500     03 RESP-KDSORT          PIC X(2).                                    
006600*                                 SORT-KOD                                
006700     03 RESP-KDFARLIG-TEXT   PIC X(15).                                   
006800     03 RESP-KVQPACK-3       PIC -(5)9.                                   
006900*                                 ANTAL I Q3 FÖRPACKNING                  
007000     03 RESP-KDKONTR         PIC Z(4)9.                                   
007100*                                 KVAL.KONTR.KOD DATAELEMENT UTG.         
007200     03 RESP-FLAR            PIC X.                                       
007300     03 RESP-BELEV1          PIC X(33).                                   
007400     03 RESP-BELEV2          PIC X(33).                                   
007500     03 RESP-ADINLOMR-PRT-ATTR                                            
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 RESP-ADINLOMR-PRT    PIC X(4).                                    
007900*                                 PRINTERPLACERING                        
008000     03 RESP-IDLOPNRM-KEY    PIC Z(8)9.                                   
008100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
008200*                                 (0VVDLLLLK)                             
008300     03 RESP-ADINLOMR        OCCURS 6 TIMES                               
008400                             PIC X(4).                                    
008500*                                 INLEVERANSOMRÅDE                        
008600*** END OF VILMAII-COPY LENGTH= 302 BYTES                                 
