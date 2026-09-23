000010*** EDIT ALLOWED                                                          
000100 01      WMSGSPAR.                                                        
000200*                                 GENERELLA SPARAREOR FÖR IMS             
000400*                                 --------------------                    
000500*                                 GENERAL SAVE-AREAS                      
000700*                                 --------------------                    
000800   03    MSG-SPAR-IDTRANS    PIC X(4)        VALUE SPACE.                 
000900*                                 BILDNR FÖR MID FRÅN VILKEN              
001000*                                 MEDDELANDET KOMMER.                     
001100*                                 SCREEN NBR OF SENDING MID.              
001200   03    MSG-SPAR-KDMFSFOR   PIC X(1)        VALUE SPACE.                 
001300*                                 TYP AV MFS-FORMAT (5:e SIFFRAN)         
001400*                                 5:th DIGIT IN SCREEN NBR.               
001600         88  SWEDISH-TEXT                    VALUE '1'.                   
001800         88  ENGLISH-TEXT                    VALUE '2'.                   
001900*                                                                         
002000   03    MSG-SPAR-KDTRTYP    PIC X(1)        VALUE SPACE.                 
002100*                                 TYP AV TRANSAKTION                      
002200*                                 (POS 7 I TRANSAKTIONSKODEN).            
002300*                                 TYPE OF TRANSACTION                     
002400*                                 (POS 7 IN TRANSACTION CODE).            
002600         88  MSG-SPAR-QUERY                  VALUE ' '.                   
002700*                                 FRÅGETRANSAKTION.                       
002800*                                 QUERY TRANSACTION.                      
003000         88  MSG-SPAR-UPDATE                 VALUE 'U'.                   
003100*                                 PF11 ÄR TRYCKT FÖR UPPDATERING.         
003200*                                 PF11 PRESSED FOR UPDATE.                
003300         88  MSG-SPAR-UPPD-V                 VALUE 'V'.                   
003400*                                 PF23 ÄR TRYCKT FÖR UPPDATERING.         
003500*                                 PF23 PRESSED FOR UPDATE.                
003600         88  MSG-SPAR-UPPD-X                 VALUE 'X'.                   
003700*                                 UPPDATERINGSTRANS FRÅN                  
003800*                                 ETT ANNAT PROGRAM.                      
003900*                                 UPDATING TRANSACTION FROM               
004000*                                 ANOTHER PROGRAM.                        
004100         88  MSG-SPAR-UPPD-Y                 VALUE 'Y'.                   
004200*                                 UPPDATERINGSTRANS FRÅN                  
004210*                                 ETT ANNAT PROGRAM.                      
004220*                                 Y-TRANS HAR ANNAN PRIORITET.            
004221*                                 UPDATING TRANSACTION FROM               
004222*                                 ANOTHER PROGRAM.                        
004224*                                                                         
004225   03    MSG-SPAR-IDPFK      PIC X(1)        VALUE SPACE.                 
004226*                                 ANGER VILKEN PFK SOM ÄR TRYCKT          
004227*                                 OM DET FINNS 2 TRANSKODER.              
004228*                                 SPECIFIES PF KEY IF THERE ARE           
004229*                                 DOUBLE TRANSACTION CODES.               
004230         88  MSG-SPAR-PRINT                  VALUE '4'.                   
004240         88  MSG-SPAR-FIRST                  VALUE '7'.                   
004250         88  MSG-SPAR-NEXT                   VALUE '8'.                   
004260         88  MSG-SPAR-ENTER                  VALUE ' '.                   
004270         88  MSG-SPAR-SPLIT                  VALUE '9'.                   
004280*                                                                         
004290   03    MSG-SPAR-MODNAMN.                                                
004300*                                 HIT FLYTTAS NAMNET PÅ DEN MOD           
004400*                                 SOM SKALL SKRIVAS.                      
004500*                                 MOVE NAME OF MOD TO BE WRITTEN          
004600*                                 TO THIS FIELD.                          
004700     05  FILLER              PIC X(6)    VALUE SPACE.                     
004800     05  MSG-SPAR-KDHUVOMR   PIC X       VALUE SPACE.                     
004900*                                 POS NR. 7 I MOD-NAMN.                   
005000*                                 SKALL VARA 0 ELLER N.                   
005100*                                 POS NO. 7 IN MOD NAME.                  
005200     05  FILLER              PIC X(1)    VALUE SPACE.                     
005400*                                                                         
030200*** END COPY WMSGSPAR    LENGTH=15    OLD LENGTH=                         
