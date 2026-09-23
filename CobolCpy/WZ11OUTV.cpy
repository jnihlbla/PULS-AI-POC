000100 01  OUTV-WZ11OUT.                                                        
000200*                                 PARAMETERS TO THE GENERAL SUB           
000300*                                 PROGRAM WZ11OUTV (VCOM/EDI)             
000400*                                 CALLS:                                  
000500*                                    USING OUTV-WZ11OUT                   
000600*                                                                         
000700*                                 IDCALL=001-9 DEPENDING ON WHICH         
000800*                                  OUTPUT "CHANNEL" IS USED.              
000900*                                 KDFUNC="OPEN", "PUT" OR "CLOSE"         
001000*                                 KDRC=0 IF OK; = 8 IF OPEN FAILS         
001100*                                                                         
001200     03 OUTV-IDCALL          PIC S9(9)           COMP.                    
001300*                                 ID OF CALL SEQUENCE                     
001400     03 OUTV-KDFUNC          PIC X(10).                                   
001500*                                 FUNCTION CODE                           
001600     03 OUTV-KDRC            PIC S9(9)           COMP.                    
001700*                                 RETURN CODE                             
001800     03 OUTV-OPEN-PARMS-FILLER.                                           
001900        05 OUTV-OPEN-PARMS.                                               
002000*                                 FIELDS USED ON OPEN CALL.               
002100           07 OUTV-IDOUTDEST PIC X(60).                                   
002200*                                 PHYSICAL OUTPUT DESTINATION             
002300           07 OUTV-TEVCOMST  PIC X(20).                                   
002400*                                 VCOM SENDERTAG                          
002500           07 OUTV-IDVCINIT  PIC X(8).                                    
002600*                                 VCOM INITIATOR PROGRAM NAME             
002700        05 FILLER            PIC X(2916).                                 
002800     03 OUTV-PUT-PARAMETERS REDEFINES OUTV-OPEN-PARMS-FILLER.             
002900*                                 FIELDS USED ON PUT CALL                 
003000        05 OUTV-TEOUTDATA-L  PIC S9(9)           COMP.                    
003100*                                 LENGTH OF DATA                          
003200        05 OUTV-TEOUTDATA    PIC X(3000).                                 
003300*                                 OUTPUT DATA                             
003400*** END OF VILMAII-COPY LENGTH= 3022 BYTES                                
