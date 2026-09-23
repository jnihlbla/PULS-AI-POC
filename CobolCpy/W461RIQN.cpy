000100 01  RIQ-W461RIQN-CTX.                                                    
000200*                                 RETURNABLE PACKING MATERIAL             
000300*                                 TO IMPORTER    RECORD TYPE RIQ          
000400     03 RIQ-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RIQ-IDDISTR          PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 RIQ-IDKUNDNR         PIC 9(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 RIQ-IDORDNR          PIC 9(7).                                    
001100*                                 ORDER NUMBER        IDORDNR-002         
001200     03 RIQ-IDDC             PIC X(2).                                    
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 RIQ-KDFAKTYP         PIC X.                                       
001500*                                 INVOICE TYPE                            
001600     03 RIQ-IDFAKT           PIC 9(7).                                    
001700*                                 INVOICE NO.                             
001800     03 RIQ-TIFAKT           PIC 9(6).                                    
001900*                                 INVOICING DATE   (YYMMDD)               
002000     03 RIQ-KDPALL           PIC X.                                       
002100*                                 PALLET TYPE                             
002200     03 RIQ-KVPALL           PIC 9(5).                                    
002300*                                 NBR OF PALLETS       KVPALL-002         
002400     03 RIQ-KVKRAG           PIC 9(5).                                    
002500*                                 NO OF FRAMES                            
002600     03 RIQ-KVLOCK           PIC 9(5).                                    
002700*                                 NO OF LIDS                              
002800     03 RIQ-FILLERX28        PIC X(28).                                   
002900*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
