000100 01  MOT-WDL221.                                                          
000200*                                 INLEVERANS HISTORIK                     
000300*                                 310 FRÅN ANDRA LAGRET                   
000400*                                 R30 CLEARING                            
000500*                                 R31 MOTTAGET                            
000600*                                 R32 RAPPORTERAD                         
000700*                                 NYCKEL SAKNAS                           
000800*                                 SÖKBEGREPP IDPTYP                       
000900*                                            IDLOPNRM                     
001000*                                            IDLEVNR                      
001100     03 MOT-IDPTYP           PIC X(3).                                    
001200*                                 POSTTYP                                 
001300     03 MOT-IDLOPNRM         PIC S9(9)           COMP-3.                  
001400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001500*                                 (0VVDLLLLK)                             
001600     03 MOT-IDAVINR          PIC S9(7)           COMP-3.                  
001700*                                 AVI-NUMMER                              
001800     03 MOT-IDANALYS         PIC X(12).                                   
001900*                                 ANALYSNUMMER                            
002000     03 MOT-IDDC             PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MOT-IDKONTO          PIC S9(11)          COMP-3.                  
002300*                                 KONTO                                   
002400     03 MOT-IDKST            PIC X(10).                                   
002500*                                 KOSTNADSSTÄLLE                          
002600     03 MOT-IDLEVNR          PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800     03 MOT-ADLAGOMR         PIC S9(3)           COMP-3.                  
002900*                                 LAGEROMRÅDE                             
003000     03 MOT-ADGANG           PIC S9(3)           COMP-3.                  
003100*                                 GÅNG                                    
003200     03 MOT-ADPLATS          PIC S9(5)           COMP-3.                  
003300*                                 LAGERPLATSNUMMER                        
003400     03 MOT-KDRT             PIC S9(3)           COMP-3.                  
003500*                                 REDOVISNINGSTYP                         
003600     03 MOT-KDAVVANT         PIC S9              COMP-3.                  
003700*                                 AVVIKELSEANTAL KOD                      
003800*                                 0=INGEN ANM.   1=AVVIKELSE              
003900*                                 2=MAKULERING AV MOTT.RAPPORT            
004000     03 MOT-KDAVVKV          PIC S9              COMP-3.                  
004100*                                 KVALITETSAVVIKELSEKOD                   
004200*                                 0=INGEN ANM.  1=AVVIKELSE               
004300*                                 2=AVVIKELSE, RETURNERAS                 
004400     03 MOT-KVANTMOT         PIC S9(7)           COMP-3.                  
004500*                                 ANTAL MOTTAGET                          
004600     03 MOT-KVAVIS           PIC S9(7)           COMP-3.                  
004700*                                 AVISERAT ANTAL                          
004800     03 MOT-KVFORDEL         PIC S9(7)           COMP-3.                  
004900*                                 ANTAL FÖRDELAT                          
005000     03 MOT-KVRETUR          PIC S9(7)           COMP-3.                  
005100*                                 ANTAL I RETUR                           
005200     03 MOT-KVFORV           PIC S9(7)           COMP-3.                  
005300*                                 FÖRVÄNTAT ANTAL EFTER JUSTERING         
005400     03 MOT-TIAVIDAT         PIC S9(7)           COMP-3.                  
005500*                                 AVISERINGSDATUM (YYMMDD)                
005600     03 MOT-TIUPPDAT         PIC S9(7)           COMP-3.                  
005700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
005800     03 MOT-IDFS             PIC X(8).                                    
005900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
006000     03 MOT-IDSHIPM          PIC 9(7).                                    
006100*                                 SKEPPNINGSNUMMER                        
006200     03 MOT-FILLER           PIC X(6).                                    
006300*** END OF VILMAII-COPY LENGTH= 107 BYTES                                 
