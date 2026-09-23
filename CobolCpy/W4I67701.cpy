000100 01  MID-W4I67701.                                                        
000200*                                 MID-COPYTEXT FÖR W40677                 
000300     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500*                                 TRANSPORT IDENTITY                      
000600     03 MID-IDLBBET-IN       PIC X(12).                                   
000700*                                 LASTBÄRARBETECKNING                     
000800*                                 TRAILER NUMBER                          
000900     03 MID-FLFARLIG-IN      PIC X.                                       
001000*                                 FARLIGT GODS-FLAGGA                     
001100*                                 DENGEROUS GOODS FLAG                    
001200     03 MID-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 MID-IDSHIPM          PIC 9(7).                                    
001600*                                 SKEPPNINGSNUMMER                        
001700*                                 SHIPMENT NO                             
001800     03 MID-FLAVSLUTA        PIC X.                                       
001900*                                 ALLMÄN FLAGGA                           
002000*                                 GENERAL FLAG                            
002100     03 MID-INDATA           OCCURS 13 TIMES.                             
002200*                                 MID-COPYTEXT FÖR W40677                 
002300        05 MID-KDCMD         PIC X.                                       
002400         88 MID-KDCMD-INGENTING                                           
002500                             VALUE ' '.                                   
002600         88 MID-KDCMD-DELETE VALUE 'D'                                    
002700                             'B'.                                         
002800         88 MID-KDCMD-REPLACE                                             
002900                             VALUE 'R'                                    
003000                             'Ä'.                                         
003100         88 MID-KDCMD-INSERT VALUE 'I'                                    
003200                             'N'.                                         
003300         88 MID-KDCMD-SELECT VALUE 'S'                                    
003400                             'V'.                                         
003500         88 MID-KDCMD-PRINT  VALUE 'P'                                    
003600                             'P'.                                         
003700*                                 RAD-UPPDATERINGSKOMMANDO                
003800*                                  BLANK  = INGENTING                     
003900*                                  D , B  = DELETE                        
004000*                                  R , Ä  = REPLACE                       
004100*                                  I , N  = INSERT                        
004200*                                  S , V  = SELECT                        
004300*                                  P , P  = PRINT                         
004400*                                 LINE UPDATE COMMAND                     
004500*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
