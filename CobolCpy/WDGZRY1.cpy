000100 01  RY1-WDGZRY1.                                                         
000200*                                 RY1                                     
000300*                                 SKAPAS VID AVVIKELSE I PACK-            
000400*                                 NINGSRAPPORTERINGEN ELLER               
000500*                                 ANNULLATIONER. ANVÄNDS VID              
000600*                                 SKAPANDE AV TRANSAKTIONER TILL          
000700*                                 ÖVRIGA SYSTEM.                          
000800     03 RY1-IDPTYP           PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 RY1-BERADREF         PIC X(10).                                   
001100*                                 KUNDENS RADREFERENS                     
001200     03 RY1-BEVOLREF         PIC X(10).                                   
001300*                                 VOLVO REFERENS                          
001400     03 RY1-IDKUNDRF         PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 RY1-IDARTNR          PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 RY1-FLRESTN          PIC X.                                       
001900*                                 RESTNOTERING ?                          
002000     03 RY1-FLDIRLEV         PIC X.                                       
002100*                                 DIREKTLEVERANS ?                        
002200     03 RY1-IDKUNDRF-RO      PIC X(10).                                   
002300*                                 KUND REF PÅ RO                          
002400     03 RY1-FLTILLK          PIC X.                                       
002500*                                 TILLKOMMANDE ARTIKEL ?                  
002600     03 RY1-KDDSP            PIC S9              COMP-3.                  
002700*                                 PÅVERKAN PÅ DSP                         
002800     03 RY1-KDFAKTYP         PIC X.                                       
002900*                                 FAKTURATYP                              
003000     03 RY1-KDORDBEK         PIC 9(2).                                    
003100*                                 ORDERBEKRÄFTELSEKOD                     
003200     03 RY1-KDORDING         PIC S9              COMP-3.                  
003300*                                 UPPDATERING ORDERINGÅNG                 
003400     03 RY1-KDORDTYP         PIC S9              COMP-3.                  
003500*                                 ORDERTYP                                
003600*                                 3 = SKROTORDER                          
003700     03 RY1-KDKVBRYT         PIC S9              COMP-3.                  
003800*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003900     03 RY1-KDTPOTYP         PIC S9              COMP-3.                  
004000*                                 TYP AV TIDPLANERAD ORDER                
004100     03 RY1-KDVRINFO         PIC S9              COMP-3.                  
004200*                                 PÅVERKAN I VR/DSP SYSTEM                
004300     03 RY1-KVBEART          PIC S9(7)           COMP-3.                  
004400*                                 BESTÄLLT ANTAL STYCKEN                  
004500     03 RY1-KVAVBART         PIC S9(7)           COMP-3.                  
004600*                                 AVBOKAT ANTAL ARTIKLAR                  
004700     03 RY1-KVAVART          PIC S9(7)           COMP-3.                  
004800*                                 AVVIKANDE ANTAL ARTIKLAR                
004900     03 RY1-KVLEVART         PIC S9(7)           COMP-3.                  
005000*                                 LEVERERAT ANTAL STYCK                   
005100     03 RY1-REKSIFFR         PIC S9              COMP-3.                  
005200*                                 KONTROLLSIFFRA                          
005300     03 RY1-TIDISPIN         PIC S9(7)           COMP-3.                  
005400*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
005500     03 RY1-TIORDREG         PIC S9(7)           COMP-3.                  
005600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005700     03 RY1-TIRODAT          PIC S9(7)           COMP-3.                  
005800*                                 RESTORDERDATUM         (ÅÅMMDD)         
005900     03 RY1-FILLER           PIC X.                                       
