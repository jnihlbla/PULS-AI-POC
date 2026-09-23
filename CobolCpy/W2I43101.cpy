000100 01  W2I43101.                                                            
000200*                                 COPYTEXT FÖR MID W2I43101               
000300     03 IDARTNR-IN           PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 IDDC-IN              PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDDC-UT              PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 INPUT.                                                            
001000*                                                                         
001100        05 IDANSK-IN         PIC 9(3).                                    
001200*                                 ANSKAFFARNUMMER                         
001300        05 IDPLANGR-AG-IN    PIC 9.                                       
001400*                                 PLANERINGSGRUPP ANSKAFFARE              
001500        05 IDINK-IN          PIC X(3).                                    
001600*                                 INKÖPARNUMMER                           
001700        05 IDLEVNR-FRAM-IN   PIC X(5).                                    
001800*                                 FRAMTIDA LEVERANTÖRNUMMER               
001900        05 IDLEVNR-SHIP-FRAM-IN                                           
002000                             PIC X(5).                                    
002100*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
002200        05 TILEVDAT-IN       PIC 9(6).                                    
002300*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
002400        05 KVSPANT-IN        PIC 9(7).                                    
002500*                                 SPÄRRAT ANTAL                           
002600        05 TIREFSTO-LOC-IN   PIC 9(6).                                    
002700*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
002800        05 FLJIT-IN          PIC X.                                       
002900*                                 JUST-IN-TIME FLAGGA                     
003000        05 FLWILSON-IN       PIC X.                                       
003100*                                 WILSONFORMEL                            
003200        05 IDREFTAB-IN       PIC X.                                       
003300*                                 IDENTITET REFILLTABELL                  
003400        05 KVSLAGER-IN       PIC 9(7).                                    
003500*                                 SÄKERHETSLAGER                          
003600        05 TIMANSEC-IN       PIC 9(6).                                    
003700*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
003800        05 KVPALL-IN         PIC 9(7).                                    
003900*                                 ANTAL I PALL                            
004000        05 KDAVT-IN          PIC 9.                                       
004100*                                 AVTALSMÄRKNING                          
004200        05 KVREFBER-IN       PIC 9(7).                                    
004300*                                 BERÄKNAD REFILLINGKVANTITET             
004400        05 TIREFPAF-IN       PIC 9(6).                                    
004500*                                 DATUM MANUELL PÅFYLLNADSKVANT           
004600        05 KVULOAD-IN        PIC 9(7).                                    
004700*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
004800        05 TISTODAT-LARM-IN  PIC 9(6).                                    
004900*                                 STOPPDATUM FÖR LARM-223                 
005000        05 KVVECKOR-LT-IN    PIC 9(2).                                    
005100*                                 ANTAL VECKOR LEDTID                     
005200        05 TIMANLED-IN       PIC 9(6).                                    
005300*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
005400        05 KVSLUTKP-IN       PIC 9(7).                                    
005500*                                 SLUTKÖPSSALDO                           
005600        05 TISLUTKP-IN       PIC 9(6).                                    
005700*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
005800        05 DISP-DAY-INPUT.                                                
005900*                                                                         
006000           07 DISP-DAY       OCCURS 5 TIMES                               
006100                             PIC X(2).                                    
006200        05 KDOPPLAN-IN       PIC X.                                       
006300*                                 OPTIMAL PLAN INOM FRYSTID               
006400        05 DAPUBL-IN         PIC 9(5).                                    
006500        05 NOTES-DATA.                                                    
006600*                                                                         
006700           07 NOTES-1        PIC X(36).                                   
006800           07 NOTES-2        PIC X(36).                                   
006900     03 SAVE-DISP-DAY.                                                    
007000*                                                                         
007100        05 SAVE-DAY          OCCURS 5 TIMES                               
007200                             PIC X(2).                                    
007300*** END OF VILMAII-COPY LENGTH= 218 BYTES                                 
