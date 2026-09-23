000100 01  DAP-WZ04DAP.                                                         
000200*                                 PARAMETERS TO PGM WZ04DAP -             
000300*                                  SUBROUTINE INTERFACE TO                
000400*                                  DISTRIBUTION & PRINT.                  
000500*                                 CALL FORMAT:                            
000600*                                  CALL WZ04DAP USING DAP-WZ04DAP         
000700*                                                                         
000800*                                 KDFUNC="OPEN", "PUT" OR "CLOSE"         
000900*                                 KDRC=0 IF OK; = 8 IF CALL FAILS         
001000*                                 BEFEL = ERROR TEXT RC > 0               
001100*                                 IDCALL=001-9 ID SEQUENCE                
001200*                                     NUMBER USED WHEN MULTIPLE           
001300*                                     PARALLEL DISTRIBUTIONS ARE          
001400*                                     ACTIVE. THE VALUE IS                
001500*                                     CREATED BY THE OPEN CALL            
001600*                                     AND USED LATER IN THE OTHER         
001700*                                     CALLS.                              
001800*                                 NOTE: AT THE MOMENT MULTIPLE            
001900*                                     PARALLEL DISTRIBUTIONS ARE          
002000*                                     NOT SUPPORTED.                      
002100*                                                                         
002200*                                 1. - KDFUNC = "OPEN"                    
002300*                                  SPECIFY IDOUTTYPE, IDOUTREC            
002400*                                  TO SELECT THE APPROPRITE D&P           
002500*                                  RULE. IDLIST IS AN OPTIONAL            
002600*                                  ID FOR THIS DISTRIBUTION.              
002700*                                  SPECIFY IDOUTDEST TO OVERRIDE          
002800*                                  VALUE GIVEN IN THE RULE.               
002900*                                                                         
003000*                                 2. - KDFUNC = "PUT"                     
003100*                                  SPECIFY TEOUTDATA AND SET              
003200*                                  KVDLEN TO ACTUAL LENGTH OF             
003300*                                  DATA.                                  
003400*                                                                         
003500*                                 3. - KDFUNC - "CLOSE"                   
003600*                                  END THIS DISTRIBUTION.                 
003700*                                                                         
003800     03 DAP-KDFUNC           PIC X(10).                                   
003900*                                 FUNCTION CODE                           
004000     03 DAP-KDRC             PIC S9(9)           COMP.                    
004100*                                 RETURN CODE                             
004200     03 DAP-BEFEL            PIC X(50).                                   
004300*                                 ERROR TEXT                              
004400     03 DAP-IDCALL           PIC S9(9)           COMP.                    
004500*                                 ID OF CALL SEQUENCE                     
004600     03 DAP-OPEN-PARMS-FILLER.                                            
004700        05 DAP-OPEN-PARMS.                                                
004800*                                 FIELDS USED ON OPEN CALL.               
004900           07 DAP-IDOUTTYPE  PIC X(15).                                   
005000*                                 OUTPUT TYPE                             
005100           07 DAP-IDOUTREC   PIC X(30).                                   
005200*                                 OUTPUT RECEIVER                         
005300           07 DAP-IDLIST     PIC X(10).                                   
005400*                                 LIST IDENTITY                           
005500           07 DAP-IDOUTDEST  PIC X(60).                                   
005600*                                 PHYSICAL OUTPUT DESTINATION             
005700        05 FILLER            PIC X(2889).                                 
005800     03 DAP-PUT-PARMS REDEFINES DAP-OPEN-PARMS-FILLER.                    
005900*                                 FIELDS USED ON PUT CALL                 
006000        05 DAP-KVDLEN        PIC S9(9)           COMP.                    
006100*                                 LENGTH OF DATA                          
006200        05 DAP-TEOUTDATA     PIC X(3000).                                 
006300*                                 OUTPUT DATA                             
006400*** END OF VILMAII-COPY LENGTH= 3072 BYTES                                
