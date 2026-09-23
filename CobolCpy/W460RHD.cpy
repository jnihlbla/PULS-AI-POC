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
001700     03 KRED-IDRADNR         PIC S9(5)           COMP-3.                  
001800*                                 LINE NO                                 
001900     03 KRED-IDORDNR         PIC S9(7)           COMP-3.                  
002000*                                 ORDER NUMBER        IDORDNR-002         
002100     03 KRED-IDKOLLI         PIC S9(5)           COMP-3.                  
002200*                                 CASE NUMBER                             
002300     03 KRED-IDARTNR         PIC S9(9)           COMP-3.                  
002400*                                 PART NUMBER                             
002500     03 KRED-REKSIFFR        PIC S9              COMP-3.                  
002600*                                 CHECK DIGIT                             
002700     03 KRED-KVLEVANM        PIC S9(7)           COMP-3.                  
002800*                                 QTY.IN DISCR.REPORT                     
002900     03 KRED-KDANMORS        PIC S9(3)           COMP-3.                  
003000*                                 DISCR.REPORT CODE                       
003100     03 KRED-KDEMBLEV        PIC S9              COMP-3.                  
003200*                                 PACK CODE DISCREP                       
003300     03 KRED-PRARTBTO        PIC S9(7)V9(2)      COMP-3.                  
003400*                                 GROSS SALES PRICE (SEK)                 
003500     03 KRED-KDFAKTYP        PIC X.                                       
003600*                                 INVOICE TYPE                            
003700     03 KRED-IDFAKT          PIC S9(7)           COMP-3.                  
003800*                                 INVOICE NO.                             
003900     03 KRED-TIFAKT          PIC S9(7)           COMP-3.                  
004000*                                 INVOICING DATE   (YYMMDD)               
004100     03 KRED-FLDIRLEV        PIC S9              COMP-3.                  
004200*                                 DIRECT DELIVERY?   FLDIRLEV-002         
004300*                                 (1 = YES)                               
004400     03 KRED-KDSPEKTO        PIC S9              COMP-3.                  
004500*                                 CODE SPECIAL ACCOUNT                    
004600     03 KRED-KDFTG           PIC S9(3)           COMP-3.                  
004700*                                 COMPANY CODE                            
004800     03 KRED-IDKONTO         PIC S9(11)          COMP-3.                  
004900*                                 ACCOUNT                                 
005000     03 KRED-FLSKROT         PIC S9              COMP-3.                  
005100     03 FILLER               PIC X(8).                                    
005200*** END COPY W460RHDCC0  LENGTH=80                                        
