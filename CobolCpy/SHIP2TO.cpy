000100* GENERATION OF COBOL HOST STRUCTURE FROM SHIP2TO-TAB                     
000200  01 SHIP2TO.                                                             
000300   03 IDSHIPM         PIC S9(7) COMP-3.                                   
000400*              SHIPMENT NO                                                
000500   03 TISKEPPN        PIC X(10).                                          
000600*              SHIPPING DATE    (YYMMDD)                                  
000700   03 TISKPTID        PIC X(8).                                           
000800   03 IDLBBET         PIC X(12).                                          
000900*              TRAILER NUMBER                                             
001000   03 IDTRPTNR        PIC S9(3) COMP-3.                                   
001100*              TRANSPORT IDENTITY                                         
001200   03 IDDC-SEND       PIC X(2).                                           
001300*              SENDING WAREHOUSE                                          
001400   03 IDPARTNER-REC   PIC X(5).                                           
001500*              REC PARTNER ID                                             
001600   03 KVKOLLI         PIC S9(5) COMP-3.                                   
001700*              NBR OF CASES                                               
001800   03 FLEXPORT        PIC X(1).                                           
001900*              EXPORT INVOICE                                             
002000   03 TITOREQ         PIC X(26).                                          
002100*              TO REQ TIMESTAMP                                           
002200   03 IDTONR          PIC X(20).                                          
002300*              TRANSPORT ORDER NO                                         
002400   03 TETORESP.                                                           
002500*              TO RESPONSE                                                
002600     49 TETORESP-L      PIC S9(4) COMP.                                   
002700*              TO RESPONSE                                                
002800     49 TETORESP-D      PIC X(5120).                                      
002900*              TO RESPONSE                                                
003000*                                                                         
003100*** END OF VILMAII-COPY LENGTH= 5215 OLD LENGTH=                          
