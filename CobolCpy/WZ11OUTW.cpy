000100 01  OUTW-WZ11OUT.                                                        
000200*                                 PARAMETERS TO THE GENERAL SUB           
000300*                                 PROGRAM WZ11OUTW (WEB OUTPUT)           
000400*                                 CALLS:                                  
000500*                                    USING OUTW-WZ11OUT                   
000600*                                                                         
000700*                                 IDCALL=001 (ONLY ONE OUTPUT             
000800*                                  "CHANNEL" IS ALLOWED)                  
000900*                                 KDFUNC="OPEN", "PUT" OR "CLOSE"         
001000*                                 KDRC=0 IF OK; = 8 IF OPEN FAILS         
001100*                                                                         
001200     03 OUTW-IDCALL          PIC S9(9)           COMP.                    
001300*                                 ID OF CALL SEQUENCE                     
001400     03 OUTW-KDFUNC          PIC X(10).                                   
001500*                                 FUNCTION CODE                           
001600     03 OUTW-KDRC            PIC S9(9)           COMP.                    
001700*                                 RETURN CODE                             
001800     03 OUTW-OPEN-PARMS-FILLER.                                           
001900        05 OUTW-OPEN-PARMS.                                               
002000*                                 FIELDS USED ON OPEN CALL.               
002100           07 OUTW-IDOUTTYPE PIC X(15).                                   
002200*                                 OUTPUT TYPE                             
002300           07 OUTW-IDOUTREC  PIC X(30).                                   
002400*                                 OUTPUT RECEIVER                         
002500           07 OUTW-IDLIST    PIC X(10).                                   
002600*                                 LIST IDENTITY                           
002700           07 OUTW-TIREGDAT  PIC S9(7)           COMP-3.                  
002800*                                 REGISTRATION DATE (YYMMDD)              
002900           07 OUTW-TIKLOCK   PIC S9(9)           COMP-3.                  
003000*                                 TIME OF DAY (HHMMSSTH)                  
003100           07 OUTW-IDLOPNR   PIC S9(3)           COMP-3.                  
003200*                                 SEQUENCE NUMBER                         
003300        05 FILLER            PIC X(2938).                                 
003400     03 OUTW-PUT-PARAMETERS REDEFINES OUTW-OPEN-PARMS-FILLER.             
003500*                                 FIELDS USED ON PUT CALL                 
003600        05 OUTW-TEOUTDATA-L  PIC S9(9)           COMP.                    
003700*                                 LENGTH OF DATA                          
003800        05 OUTW-TEOUTDATA    PIC X(3000).                                 
003900*                                 OUTPUT DATA                             
004000*** END OF VILMAII-COPY LENGTH= 3022 BYTES                                
