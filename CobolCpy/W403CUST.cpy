000100 01  CUST-W403CUST.                                                       
000200*                                 3IV CUSTOMER SUMMARY POST               
000300*                                 3IV CUSTOMER SUMMARY RECORD             
000400*                                 IDRTYP3IV="CustomerSummary"             
000500     03 CUST-IDRTYP3IV       PIC X(30).                                   
000600*                                 3IV RECORD-TYP                          
000700*                                 3IV RECORD TYPE                         
000800     03 CUST-IDPRODNR        PIC Z(6)9.                                   
000900*                                 PRODUKTIONSNUMMER                       
001000*                                 PRODUCTION NUMBER                       
001100     03 CUST-IDPLKLST        PIC 9(3).                                    
001200*                                 PLOCKLISTNUMMER                         
001300*                                 PICKING LIST NUMBER                     
001400     03 CUST-IDDISTR         PIC Z(3)9.                                   
001500*                                 DISTRIKTNUMMER                          
001600*                                 DISTRICT NUMBER                         
001700     03 CUST-IDKUNDNR        PIC Z(5)9.                                   
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000     03 CUST-IDORDNR5        PIC Z(4)9.                                   
002100*                                 ORDERNUMMER                             
002200*                                 ORDER NUMBER                            
002300     03 CUST-IDPRC.                                                       
002400*                                 PRODUKTIONSKANAL                        
002500*                                 PRODUCTION CHANNEL                      
002600        05 CUST-IDPRCBAS     PIC X(3).                                    
002700*                                 PRC-BAS                                 
002800*                                 PRC-BASIC                               
002900        05 CUST-IDPRCVAR     PIC X.                                       
003000*                                 PRC-VARIANT                             
003100*                                 PRC-VARIANT                             
003200     03 CUST-IDLOPNR-ORD     PIC Z(2)9.                                   
003300*                                 ORDERNS ORDNINGSNUMMER INOM             
003400*                                 EN PLOCKSATS                            
003500*                                 SEQUENCE-NUMBER FOR AN ORDER            
003600*                                 WITHIN A PICKING UNIT                   
003700     03 CUST-KDEMBTYP        PIC 9.                                       
003800*                                 EMBALLAGETYP                            
003900*                                 PACKAGE TYPE                            
004000     03 CUST-BEEMBTYP        PIC X(12).                                   
004100*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
004200     03 CUST-DUMMY-CHDIG     PIC Z.                                       
004300     03 CUST-KVRADER         PIC Z(4)9.                                   
004400*                                 ANTAL RADER                             
004500*                                 NUMBER OF LINES                         
004600     03 CUST-VKORDNTO        PIC Z(5)9.9.                                 
004700*                                 ORDERVIKT NETTO (KG)                    
004800*                                 WEIGHT PER ORDER NETTO (KG)             
004900     03 CUST-VLORDNTO        PIC Z(3)9.9(3).                              
005000*                                 ORDERVOLYM NETTO (M3)                   
005100*                                 NET VOLUME PER ORDER (M3)               
005200     03 CUST-FLPMT3IV        PIC X.                                       
005300*                                 LÄS UPP INSTRUKTIONER?                  
005400*                                 PROMT INSTRUCTIONS?                     
005500     03 CUST-BELAGINS-GRP.                                                
005600*                                 LAGERINSTRUKTIONER                      
005700*                                 WAREHOUSE INSTRUCTIONS                  
005800        05 CUST-BELAGINS-DEL1                                             
005900                             PIC X(60).                                   
006000*                                 DEL AV LAGERINSTRUKTION                 
006100*                                 PART OF WAREHOUSE INSTRUCTIONS          
006200        05 CUST-BELAGINS-DEL2                                             
006300                             PIC X(60).                                   
006400*                                 DEL AV LAGERINSTRUKTION                 
006500*                                 PART OF WAREHOUSE INSTRUCTIONS          
006600     03 CUST-BEFDKRAV        PIC X(40).                                   
006700*                                 FÖRRÅDSDATAKRAV                         
006800*                                 PACKING INSTRUCTIONS                    
006900     03 CUST-FLURSRAP        PIC X.                                       
007000*                                 URSPRUNGSRAPPORTERING VID               
007100*                                 PACKNING                                
007200*                                 FLAG FOR REPORTING OF ORIGIN            
007300*** END OF VILMAII-COPY LENGTH= 259 BYTES                                 
