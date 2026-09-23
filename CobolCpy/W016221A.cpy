000100 01  221A-W016221A.                                                       
000200*                                 COPYTEXT FÖR VCOM-SÄNDNING              
000300*                                 KVLRECL = RECORDLÄNGD TILL VCOM         
000400*                                 KDRECFM = RECORDFORMAT TO VCOM          
000500     03 221A-IDVCOM.                                                      
000600*                                 VCOM IDENTITET                          
000700*                                 VCOM IDENTITY                           
000800        05 221A-VC-IDSYSTEM  PIC X(4).                                    
000900*                                 VOLVO VCAS SYSTEMNUMMER                 
001000*                                 VOLVO VCAS SYSTEM NUMBER                
001100        05 221A-VC-IDVCOMLOP PIC X(2).                                    
001200*                                 VCOM IDENTITET LÖPNUMMER                
001300*                                 VCOM IDENTITY LOPNUMBER                 
001400        05 221A-VC-IDLANDX2  PIC X(2).                                    
001500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001600*                                 2-LETTER CODE FOR COUNTRY               
001700     03 221A-FILLER          PIC X.                                       
001800     03 221A-IDCPYTXT.                                                    
001900*                                 COPYTEXT IDENTITET                      
002000*                                 IDENTITY OF A COPYTEXT                  
002100        05 221A-CT-IDSYSTEM  PIC X(4).                                    
002200*                                 VOLVO VCAS SYSTEMNUMMER                 
002300*                                 VOLVO VCAS SYSTEM NUMBER                
002400        05 221A-CT-IDPTYP    PIC X(3).                                    
002500*                                 POSTTYP                                 
002600*                                 RECORD TYPE                             
002700        05 221A-CT-IDVTYP    PIC X.                                       
002800*                                 POSTTYPSVERSION                         
002900*                                 RECORD TYPE VERSION                     
003000     03 221A-FILLER          PIC X.                                       
003100     03 221A-KVLRECL         PIC 9(4).                                    
003200*                                 LRECL I ETT VARIABELT RECORD            
003300*                                 LRECL I A VARIABLE RECORD               
003400     03 221A-FILLER          PIC X.                                       
003500     03 221A-KDRECFM         PIC X(3).                                    
003600*                                 RECORD-FORMAT I JCL                     
003700*                                 RECORD FORMAT IN JCL                    
003800     03 221A-FILLER          PIC X.                                       
003900     03 221A-TEVCOMST        PIC X(20).                                   
004000*                                 VCOM SENDERTAG                          
004100*                                 VCOM SENDERTAG                          
004200     03 221A-FILLER          PIC X.                                       
004300     03 221A-IDVCINIT        PIC X(8).                                    
004400*                                 VCOM INITIATOR PROGRAM NAMN             
004500*                                 VCOM INITIATOR PROGRAM NAME             
004600     03 221A-FILLER          PIC X(4).                                    
004700     03 221A-NOTERING        PIC X(20).                                   
004800*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
