000100 01  KRED-W460RHD.                                                        
000200*                                 CRED TRANS. FROM VIPS                   
000300*                                 TO NOAC RECORD TYPE RHD                 
000400     03 KRED-IDPTYP          PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 KRED-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRICT NUMBER                         
000800     03 KRED-IDKUNDNR        PIC S9(7)           COMP-3.                  
000900*                                 CUSTOMER NO                             
001000     03 KRED-KDCLAGER        PIC S9              COMP-3.                  
001100*                                 CENTRAL WAREHOUSE CODE                  
001200     03 KRED-KDFRAKT         PIC S9(3)           COMP-3.                  
001300*                                 FREIGHT CODE                            
001400     03 KRED-IDLEVANM        PIC X(7).                                    
001500*                                 DISCREPANCY REPORT NO                   
001600     03 KRED-TIM-LEVANM      PIC S9(7)           COMP-3.                  
001700     03 KRED-IDORDNR         PIC S9(7)           COMP-3.                  
001800*                                 ORDER NUMBER        IDORDNR-002         
001900     03 KRED-IDKOLLI         PIC S9(5)           COMP-3.                  
002000*                                 CASE NUMBER                             
002100     03 KRED-IDARTNR         PIC S9(9)           COMP-3.                  
002200*                                 PART NUMBER                             
002300     03 KRED-REKSIFFR        PIC S9              COMP-3.                  
002400*                                 CHECK DIGIT                             
002500     03 KRED-KVLEVANM        PIC S9(7)           COMP-3.                  
002600*                                 QTY.IN DISCR.REPORT                     
002700     03 KRED-KDANMORS        PIC S9(3)           COMP-3.                  
002800*                                 DISCR.REPORT CODE                       
002900     03 KRED-KDEMBLEV        PIC S9              COMP-3.                  
003000*                                 PACK CODE DISCREP                       
003100     03 KRED-PRARTBTO        PIC S9(7)V9(2)      COMP-3.                  
003200*                                 GROSS SALES PRICE (SEK)                 
003300     03 KRED-KDFAKTYP        PIC X.                                       
003400*                                 INVOICE TYPE                            
003500     03 KRED-IDFAKT          PIC S9(7)           COMP-3.                  
003600*                                 INVOICE NO.                             
003700     03 KRED-TIFAKT          PIC S9(7)           COMP-3.                  
003800*                                 INVOICING DATE   (YYMMDD)               
003900     03 KRED-FLDIRLEV        PIC S9              COMP-3.                  
004000*                                 DIRECT DELIVERY?   FLDIRLEV-002         
004100*                                 (1 = YES)                               
004200     03 KRED-KDSPEKTO        PIC S9              COMP-3.                  
004300*                                 CODE SPECIAL ACCOUNT                    
004400     03 KRED-KDFTG           PIC S9(3)           COMP-3.                  
004500*                                 COMPANY CODE                            
004600     03 KRED-IDKONTO         PIC S9(11)          COMP-3.                  
004700*                                 ACCOUNT                                 
004800     03 KRED-FLSKROT         PIC S9              COMP-3.                  
004900     03 FILLER               PIC X(11).                                   
005000*** END COPY W460RHDMC0  LENGTH=80                                        
