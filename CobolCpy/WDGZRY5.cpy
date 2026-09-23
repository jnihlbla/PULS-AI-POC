000100 01  RY5-WDGZRY5.                                                         
000200*                                 RY5                                     
000300*                                 SKAPAS VID ANNULLATION AV               
000400*                                 UTSKRIVNA EJ PACKNINGSRAPPOR-           
000500*                                 TERADE RADER. ANVÄNDS VID               
000600*                                 SKAPANDE AV TRANSAKTIONER TILL          
000700*                                 ÖVRIGA SYSTEM.                          
000800     03 RY5-IDPTYP           PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 RY5-BERADREF         PIC X(10).                                   
001100*                                 KUNDENS RADREFERENS                     
001200     03 RY5-BEVOLREF         PIC X(10).                                   
001300*                                 VOLVO REFERENS                          
001400     03 RY5-IDKUNDRF         PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 RY5-IDARTNR          PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 RY5-FLRESTN          PIC X.                                       
001900*                                 RESTNOTERING ?                          
002000     03 RY5-FLDIRLEV         PIC X.                                       
002100*                                 DIREKTLEVERANS ?                        
002200     03 RY5-FLLSBOK          PIC X.                                       
002300*                                 LAGERAVBOKNING                          
002400     03 RY5-FLORDSPE         PIC X.                                       
002500*                                 SPECIALORDERFLAGGA                      
002600     03 RY5-IDKUNDRF-RO      PIC X(10).                                   
002700*                                 KUND REF PÅ RO                          
002800     03 RY5-KDARTERS         PIC S9              COMP-3.                  
002900*                                 ERSÄTTNINGSKOD W415                     
003000     03 RY5-IDDC             PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200     03 RY5-KDDSP            PIC S9              COMP-3.                  
003300*                                 PÅVERKAN PÅ DSP                         
003400     03 RY5-KDFAKTYP         PIC X.                                       
003500*                                 FAKTURATYP                              
003600     03 RY5-KDFRAKT          PIC S9(3)           COMP-3.                  
003700*                                 FRAKTSÄTT C1-C2 TILL KUND               
003800     03 RY5-KDORDING         PIC S9              COMP-3.                  
003900*                                 UPPDATERING ORDERINGÅNG                 
004000     03 RY5-KDORDKL          PIC S9              COMP-3.                  
004100*                                 ORDERKLASS                              
004200     03 RY5-KDORDKL-URS      PIC S9              COMP-3.                  
004300*                                 URSPRUNGLIG ORDERKLASS                  
004400     03 RY5-KDORDTYP         PIC S9              COMP-3.                  
004500*                                 ORDERTYP                                
004600*                                 3 = SKROTORDER                          
004700     03 RY5-KDKVBRYT         PIC S9              COMP-3.                  
004800*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004900     03 RY5-KDVRINFO         PIC S9              COMP-3.                  
005000*                                 PÅVERKAN I VR/DSP SYSTEM                
005100     03 RY5-KVBEART          PIC S9(7)           COMP-3.                  
005200*                                 BESTÄLLT ANTAL STYCKEN                  
005300     03 RY5-KVAVBART         PIC S9(7)           COMP-3.                  
005400*                                 AVBOKAT ANTAL ARTIKLAR                  
005500     03 RY5-KVAVART          PIC S9(7)           COMP-3.                  
005600*                                 AVVIKANDE ANTAL ARTIKLAR                
005700     03 RY5-KVANNANT         PIC S9(7)           COMP-3.                  
005800*                                 ANNULLERAT ANTAL ARTIKLAR               
005900     03 RY5-REKSIFFR         PIC S9              COMP-3.                  
006000*                                 KONTROLLSIFFRA                          
006100     03 RY5-TIORDREG         PIC S9(7)           COMP-3.                  
006200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
006300     03 RY5-TIRODAT          PIC S9(7)           COMP-3.                  
006400*                                 RESTORDERDATUM         (ÅÅMMDD)         
006500*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
