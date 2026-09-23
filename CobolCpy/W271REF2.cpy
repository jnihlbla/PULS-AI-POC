000100 01  REF2-W271REF2.                                                       
000200*                                 LÄNKAREA TILL W271REF2.                 
000300*                                                                         
000400*                                 INSTRUKTION:                            
000500*                                                                         
000600*                                 FYLL I INDATA:                          
000700*                                 LAGER        (OBLIGATORISKT)            
000800*                                 PERIODBEHOV  (OBLIGATORISKT)            
000900*                                 STANDARDPRIS (OBLIGATORISKT)            
001000*                                 OM MANUELL PÅFYLLNADSPUNKT ÄR           
001100*                                 SATT FYLL I IN-KVREFPKT                 
001200*                                 OM MANUELL PÅFYLLNADSKVANT ÄR           
001300*                                 SATT FYLL I IN-KVREFBER                 
001400*                                 REFILLTABELL (OBLIGATORISKT)            
001500*                                 WILSONFLAGGA (OBLIGATORISKT)            
001600*                                 BEHOVS/BINDNINGSDATUM (OBL)             
001700*                                 SÄSONGSINDEX (OBLIGATORISKT)            
001800*                                                                         
001900*                                 ANROPA W271REF2 USING                   
002000*                                        REF2-W271REF2                    
002100*                                                                         
002200*                                 OM LAGRET OK (KDSVAR=SPACE),            
002300*                                 FINNS I SVARET UPPGIFTER OM             
002400*                                 LAGRET SAMT REFILL-INFO FÖR             
002500*                                 ANGIVEN KLASS AV ARTIKEL                
002600*                                 (STANDARDPRIS/PERIODBEHOV).             
002700*                                                                         
002800     03 REF2-IN-UTDATA.                                                   
002900        05 REF2-INDATA.                                                   
003000           07 REF2-IDDC      PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200           07 REF2-IDREFTAB  PIC X.                                       
003300*                                 IDENTITET REFILLTABELL                  
003400           07 REF2-FLWILSON  PIC X.                                       
003500*                                 WILSONFORMEL                            
003600           07 REF2-PRARTBES  PIC S9(7)V9(2)      COMP-3.                  
003700*                                 BESTÄLLNINGSPRIS I KRONOR               
003800           07 REF2-KVPB-REF  PIC S9(6)V9(1)      COMP-3.                  
003900*                                 PERIODBEHOV REFILLING                   
004000           07 REF2-IN-KVREFPKT                                            
004100                             PIC S9(7)           COMP-3.                  
004200*                                 BERÄKNAD PÅFYLLNADSPUNKT                
004300           07 REF2-IN-KVREFBER                                            
004400                             PIC S9(7)           COMP-3.                  
004500*                                 BERÄKNAD REFILLINGKVANTITET             
004600           07 REF2-BINNDAY-TIAAMMDD                                       
004700                             PIC 9(6).                                    
004800           07 REF2-RESEASON  OCCURS 12 TIMES                              
004900                             PIC S9V9(2)         COMP-3.                  
005000*                                 SÄSONGSINDEX                            
005100           07 REF2-FLFLYG    PIC X.                                       
005200*                                 FLYGARTIKEL                             
005300        05 REF2-UTDATA.                                                   
005400           07 REF2-KVREFPKT  PIC S9(7)           COMP-3.                  
005500*                                 BERÄKNAD PÅFYLLNADSPUNKT                
005600           07 REF2-KVREFBER  PIC S9(7)           COMP-3.                  
005700*                                 BERÄKNAD REFILLINGKVANTITET             
005800        05 REF2-KDSVAR       PIC X.                                       
005900*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
006000*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
