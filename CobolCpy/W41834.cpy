000100 01  W41834.                                                              
000200*                                 UPPDATERINGSPOSTER TILL W41835          
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 LEVANM-NYCKLAR.                                                   
000600*                                 NYCKLAR TILL WDA2                       
000700        05 IDLEVANM.                                                      
000800*                                 LEVERANSANMÄRKNINGSIDENTITET            
000900           07 IDDISTR        PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100           07 IDKUNDNR       PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300           07 IDRAPPNR       PIC 9(7).                                    
001400*                                 RAPPORT NUMMER                          
001500        05 IDARTNR           PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700        05 IDRADNR           PIC S9(5)           COMP-3.                  
001800*                                 RADNUMMER                               
001900     03 IDDC                 PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 FLFARLIG             PIC X.                                       
002200*                                 FARLIGT GODS-FLAGGA                     
002300     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
002400*                                 KREDITNOTANUMMER                        
002500     03 IDPERSON             PIC S9(3)           COMP-3.                  
002600*                                 PERSONKOD                               
002700     03 KDARBTYP             PIC X(8).                                    
002800*                                 TYP AV ARBETE                           
002900     03 KDAVVTYP             PIC S9              COMP-3.                  
003000*                                 AVVIKELSETYP                            
003100*                                 1=POSITIV.  2=NEGATIV                   
003200     03 KDFAKTYP-KNOT        PIC X.                                       
003300*                                 FAKTURATYP KREDITNOTA                   
003400     03 KDLEVANM             PIC X.                                       
003500*                                 STATUS LEVERANSANMÄRKNING               
003600     03 KVLEVANM             PIC S9(7)           COMP-3.                  
003700*                                 LEVERANSANMÄRKNINGSANTAL                
003800     03 KVRADER-RT           PIC S9(5)           COMP-3.                  
003900*                                 ANTAL RADER RETURTILLSTÅND              
004000     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
004100*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
004200     03 TIKNOTA              PIC S9(7)           COMP-3.                  
004300*                                 KREDITNOTADATUM                         
004400     03 TIRETILL             PIC S9(7)           COMP-3.                  
004500*                                 RETURTILLSTÅNDSDATUM                    
004600     03 IDDC-RET             PIC X(2).                                    
004700*                                 MOTTAGANDE LAGER FÖR RETURER            
004800     03 IXDCCLEAR            PIC 9.                                       
004900*                                 CLEARING DC SEKVENS                     
005000     03 INVENTERINGSDATA.                                                 
005100*                                 FÄLT FÖR UPPDATERING AV WDH1            
005200        05 KDINVKAT          PIC S9(3)           COMP-3.                  
005300*                                 INVENTERINGSKATEGORI                    
005400        05 KVJUSTKV          PIC S9(7)           COMP-3.                  
005500*                                 JUSTERAD KVANTITET                      
005600        05 TIM-INV           PIC S9(7)           COMP-3.                  
005700*                                 DATUM FÖR INV. ANMODAN (ÅÅMMDD)         
005800        05 TEINVANM          PIC X(25).                                   
005900*                                 INVENTERINGSANMÄRKNING                  
006000*** END OF VILMAII-COPY LENGTH= 103 BYTES                                 
