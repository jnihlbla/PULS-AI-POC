000100 01  RIN-W461RINN-CTX.                                                    
000200*                                 INVOICE CASE TO IMPORTER                
000300*                                 RECORD TYPE RIN                         
000400     03 RIN-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RIN-IDKUNDNR         PIC 9(6).                                    
000700*                                 CUSTOMER NO                             
000800     03 RIN-IDORDNR          PIC 9(7).                                    
000900*                                 ORDER NUMBER        IDORDNR-002         
001000     03 RIN-IDPRODNR         PIC 9(7).                                    
001100*                                 PRODUCTION NUMBER                       
001200     03 RIN-IDKOLLI          PIC 9(5).                                    
001300*                                 CASE NUMBER                             
001400     03 RIN-VKORDBTO-KOLLI   PIC 9(6)V9(1).                               
001500*                                 ORDER WEIGHT GROSS PER CASE             
001600     03 RIN-VLORDBTO-KOLLI   PIC 9(4)V9(3).                               
001700*                                 ORDER VOL GR/CASE                       
001800     03 RIN-IDLBBET          PIC X(12).                                   
001900*                                 TRAILER NUMBER                          
002000     03 RIN-KDEMBTYP         PIC 9(2).                                    
002100*                                 PACKAGE TYPE       KDEMBTYP-002         
002200     03 RIN-IDFAKT-GNB       PIC X(8).                                    
002300*                                 INVOICE NO. GNB                         
002400     03 RIN-FILLERX16        PIC X(16).                                   
002500*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
