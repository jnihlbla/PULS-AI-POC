000100 01  RESP-WL0124O1.                                                       
000200     03 RESP-IDDC-KEY        PIC X(2).                                    
000300*                                 WAREHOUSE IDENTIFIER                    
000400     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000500*                                 DISTRICT NUMBER                         
000600     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000700*                                 CUSTOMER NO                             
000800     03 RESP-IDORDNR7-KEY    PIC Z(6)9.                                   
000900*                                 ORDER NUMBER                            
001000     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001100*                                 PRODUCTION NUMBER                       
001200     03 RESP-IDTRP-IN.                                                    
001300*                                 TRANSPORTIDENTITY                       
001400        05 RESP-IDTRPLOS     PIC X(3).                                    
001500*                                 TRANSPORTSOLUTION                       
001600        05 RESP-IDTRPVAR     PIC X(2).                                    
001700*                                 TRANSPORTSOLUTIONGROUP                  
001800     03 RESP-TITRPAVG.                                                    
001900        05 RESP-TITRPAVG-DAT PIC 9(6).                                    
002000*                                 YEAR - MONTH - DAY  (YYMMDD)            
002100        05 RESP-TITRP-FILLER PIC X.                                       
002200        05 RESP-TITRPAVG-TID PIC 9(4).                                    
002300*                                 TIME IN HOUR AND MINUTE                 
002400     03 RESP-KDFRAKT         PIC Z9.                                      
002500*                                 FREIGHT CODE                            
002600     03 RESP-IDPLKLST-UPD    PIC Z(2)9.                                   
002700*                                 PICKING LIST NUMBER                     
002800     03 RESP-TIAAMMDD-UPD    PIC 9(6).                                    
002900*                                 YEAR - MONTH - DAY  (YYMMDD)            
003000     03 RESP-TIHHMM-UPD      PIC 9(4).                                    
003100*                                 TIME IN HOUR AND MINUTE                 
003200     03 RESP-FLJANEJ         PIC X.                                       
003300     03 RESP-KVRADER         PIC Z(4)9.                                   
003400*                                 NUMBER OF LINES                         
003500     03 RESP-LINE            OCCURS 500 TIMES.                            
003600        05 RESP-IDPLKLST     PIC Z(2)9.                                   
003700*                                 PICKING LIST NUMBER                     
003800        05 RESP-IDPRC.                                                    
003900*                                 PRODUCTION CHANNEL                      
004000           07 RESP-IDPRCBAS  PIC X(3).                                    
004100*                                 PRC-BASIC                               
004200           07 RESP-IDPRCVAR  PIC X.                                       
004300*                                 PRC-VARIANT                             
004400        05 RESP-KDODELSTA    PIC X.                                       
004500*                                 ORDER PART STATUS                       
004600        05 RESP-KVRADER-LINE PIC Z(4)9.                                   
004700*                                 NUMBER OF LINES                         
004800        05 RESP-KVPACKRAD-OD PIC Z(4)9.                                   
004900*                                 NUMBER OF PACKED RADER                  
005000        05 RESP-VKORDNTO     PIC Z(4)9.9.                                 
005100*                                 WEIGHT PER ORDER NETTO (KG)             
005200        05 RESP-VLORDNTO     PIC Z(2)9.9(3).                              
005300*                                 NET VOLUME PER ORDER (M3)               
005400        05 RESP-TILST-OD.                                                 
005500           07 RESP-TILST-DAT PIC 9(6).                                    
005600*                                 YEAR - MONTH - DAY  (YYMMDD)            
005700           07 RESP-TILST-FILLER                                           
005800                             PIC X.                                       
005900           07 RESP-TILST-TID PIC 9(4).                                    
006000*                                 TIME IN HOUR AND MINUTE                 
006100        05 RESP-SUPTID       PIC Z(2)9.9(2).                              
006200*                                 TOTAL PRODUCTIONTIME HOUR MIN.          
006300        05 RESP-TIRFS-OUT.                                                
006400           07 RESP-TIRFS-DAT PIC 9(6).                                    
006500*                                 YEAR - MONTH - DAY  (YYMMDD)            
006600           07 RESP-TIRFS-FILLER                                           
006700                             PIC X.                                       
006800           07 RESP-TIRFS-TID PIC 9(4).                                    
006900*                                 TIME IN HOUR AND MINUTE                 
007000        05 RESP-IDPRCPLK     PIC X(4).                                    
007100*                                 ID FOR A PICKING UNIT                   
007200        05 RESP-IDLOTNR-PLK  PIC 9(3).                                    
007300*                                 ORDERLOT NUMBER FOR A PICKLIST          
007400        05 RESP-IDUSER-LINE  PIC X(8).                                    
007500*                                 USER SECURITY-IDENTITY                  
007600        05 RESP-IDMSG-ERROR-LINE                                          
007700                             PIC X(3).                                    
007800*                                 ERROR MESSAGE ID                        
007900*** END OF VILMAII-COPY LENGTH= 39063 BYTES                               
