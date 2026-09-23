000100 01  W42681.                                                              
000200*                                 INLEVERANS HISTORIK                     
000300*                                 310 FRÅN ANDRA LAGRET                   
000400*                                 R30 CLEARING                            
000500*                                 R31 MOTTAGET                            
000600*                                 R32 RAPPORTERAD                         
000700     03 IDPTYP               PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
001000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001100*                                 (0VVDLLLLK)                             
001200     03 IDAVINR              PIC S9(7)           COMP-3.                  
001300*                                 AVI-NUMMER                              
001400     03 IDKONTO              PIC S9(11)          COMP-3.                  
001500*                                 KONTO                                   
001600     03 IDLEVNR              PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001900*                                 LAGEROMRÅDE                             
002000     03 ADGANG               PIC S9(3)           COMP-3.                  
002100*                                 GÅNG                                    
002200     03 ADPLATS              PIC S9(5)           COMP-3.                  
002300*                                 LAGERPLATSNUMMER                        
002400     03 KDCLAGER             PIC S9              COMP-3.                  
002500*                                 CENTRALLAGERKOD                         
002600     03 KDRT                 PIC S9(3)           COMP-3.                  
002700*                                 REDOVISNINGSTYP                         
002800     03 KDAVVANT             PIC S9              COMP-3.                  
002900*                                 AVVIKELSEANTAL KOD                      
003000*                                 0=INGEN ANM.   1=AVVIKELSE              
003100*                                 2=MAKULERING AV MOTT.RAPPORT            
003200     03 KDAVVKV              PIC S9              COMP-3.                  
003300*                                 KVALITETSAVVIKELSEKOD                   
003400*                                 0=INGEN ANM.  1=AVVIKELSE               
003500*                                 2=AVVIKELSE, RETURNERAS                 
003600     03 KVANTMOT             PIC S9(7)           COMP-3.                  
003700*                                 ANTAL MOTTAGET                          
003800     03 KVAVIS               PIC S9(7)           COMP-3.                  
003900*                                 AVISERAT ANTAL                          
004000     03 KVFORDEL             PIC S9(7)           COMP-3.                  
004100*                                 ANTAL FÖRDELAT                          
004200     03 KVRETUR              PIC S9(7)           COMP-3.                  
004300*                                 ANTAL I RETUR                           
004400     03 KVFORV               PIC S9(7)           COMP-3.                  
004500*                                 FÖRVÄNTAT ANTAL EFTER JUSTERING         
004600     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
004700*                                 AVISERINGSDATUM (YYMMDD)                
004800     03 TIUPPDAT             PIC S9(7)           COMP-3.                  
004900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
005000*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  
