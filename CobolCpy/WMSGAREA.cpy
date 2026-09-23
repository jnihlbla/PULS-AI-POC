000010*** EDIT ALLOWED                                                          
000100 01    MSG-IO-AREA.                                                       
000500*                                    * DESCRIPTION OF GENERAL             
000600*                                    * MID AND MOD AREA                   
000700   03    MSG-KVLL                PIC S9(4)         COMP SYNC.             
000900*                                    * LENGTH OF MESSAGE                  
000913   03    MSG-KDZ1                PIC X.                                   
000915*                                    * FLAG FOR MFS                       
000919   03    MSG-KDZ2                PIC X.                                   
000930*                                    * FLAG FOR MFS                       
000970   03    MSG-AREA.                                                        
000990*                                    * THE MESSAGE ITSELF                 
001000     05    MSG-KDTRANS-1.                                                 
001010*                                    * TRANSACTION CODE                   
001200*                                    * (EV. FRÅN FP-TANGENT)              
001400*                                    * (PERHAPS FROM F-KEY)               
001500       07  FILLER                PIC X(6).                                
001600       07  MSG-KDTRTYP           PIC X(1).                                
001700*                                    * TRANSAKTIONSTYP                    
002100       07  MSG-IDPFK             PIC X(1).                                
002110*                                    * MFS-PFK-TANGENT                    
002300     05    MSG-IDTRANS-1         PIC X(4).                                
002400*                                    * ANGER FRÅN VILKEN MID              
002500*                                    * MEDDELANDET KOMMER                 
002600*                                    * THE MID FROM WHICH THE             
002700*                                    * MESSAGE ORIGINATES                 
002800     05    MSG-KDMFSFOR-1        PIC X(1).                                
002900*                                    * MFS-FORMAT (OM EN TRANSKOD)        
003000*                                    * (IF ONE TRANS CODE)                
003100*                                    * 1 SVENSKA LEDTEXTER                
003200*                                    * 2 ENGELSKA LEDTEXTER               
003300     05    MSG-INDATA-MINUS-1-TRANSACT.                                   
003400       07  MSG-INDATA-MINUS-1-TRANSKOD     PIC X(1920).                   
003500*                                    * MID-INPUT                          
003600*                                    * NÄR PFK EJ ÄR TRYCKT               
003700*                                    * WHEN F-KEY NOT PRESSED             
003800   03    FILLER REDEFINES MSG-AREA.                                       
003900     05    FILLER                PIC X(8).                                
004000     05    MSG-KDTRANS-2.                                                 
004100*                                    * MIDDENS TRANSKOD                   
004200*                                    * NÄR PF ÄR TRYCKT                   
004300*                                    * TRANS CODE OF THE MID              
004400*                                    * WHEN F-KEY PRESSED                 
004500       07    FILLER              PIC X.                                   
004600         88  MSG-DUBBLA-TRANSKODER            VALUE 'R' 'W'.              
004700         88  MSG-DOUBLE-TRANSACTIONS          VALUE 'R' 'W'.              
004800*                                    * FÖR TEST                           
004900*                                    * OM DUBBLA TRANSKODER               
005000*                                    * TEST FOR DOUBLE TRANS              
005100       07    FILLER              PIC X(7).                                
005500*                                                                         
005600     05    MSG-IDTRANS-2         PIC X(4).                                
005700*                                    * ANGER FRÅN VILKEN MID              
005800*                                    * MEDDELANDET KOMMER                 
005900*                                    * THE MID FROM WHICH THE             
006000*                                    * MESSAGE ORIGINATES                 
006100     05    MSG-KDMFSFOR-2        PIC X(1).                                
006200*                                    * MFS-FORMAT                         
006300*                                    * (OM TVÅ TRANSKODER)                
006400*                                    * (IF TWO TRANS CODES)               
006500*                                    * 1 SVENSKA LEDTEXTER                
006600*                                    * 2 ENGELSKA LEDTEXTER               
006700     05    MSG-INDATA-MINUS-2-TRANSACT.                                   
006800       07  MSG-INDATA-MINUS-2-TRANSKODER   PIC X(1912).                   
006900*                                    * MID-INPUT NÄR PF ÄR TRYCKT         
006910   03    FILLER REDEFINES MSG-AREA.                                       
006920     05    FILLER                PIC X(13).                               
006930     05    MSG-MID-OUT           PIC X(1920).                             
006940*                                    * MIDDENS COPYTEXT VID               
006950*                                    * PROGRAM-TO-PROGRAM SW              
007100*** END COPY WMSGAREA  LENGTH=1937  OLD LENGTH=1937                       
