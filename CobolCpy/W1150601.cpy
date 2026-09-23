000100 01  W11506.                                                              
000200     03 SEQUENCE-GRP         OCCURS 4 TIMES.                              
000300        05 SEQUENCE          PIC X(10).                                   
000400     03 IDPROJ               PIC X(4).                                    
000500*                                 PARTS PROJECT IDENTITY                  
000600     03 KDBASLM              PIC X(6).                                    
000700*                                 BASIC STOCK MARKET                      
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRICT NUMBER                         
001000     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001100*                                 FUNCTION GROUP                          
001200     03 IDARTNUMMER.                                                      
001300        05 IDARTNR           PIC S9(9)           COMP-3.                  
001400*                                 PART NUMBER                             
001500        05 STRECK            PIC X.                                       
001600        05 REKSIFFR          PIC 9.                                       
001700*                                 PART NO CHECK DIGIT                     
001800     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001900*                                 PRODUCT GROUP                           
002000     03 PRIS-AKTUELL-ART     PIC S9(7)V9(2)      COMP-3.                  
002100*                                 GROSS SALES PRICE (SEK)                 
002200     03 SUM-AKTUELL-ART      PIC S9(9)V9(2)      COMP-3.                  
002300     03 BEART                PIC X(25).                                   
002400*                                 PART DESCRIPTION                        
002500     03 KDBPSR               PIC S9              COMP-3.                  
002600*                                 BASIC PART STOCK RECOMMENDATION         
002700     03 TISTOMREG            PIC S9(7)           COMP-3.                  
002800*                                 STOP-TIME MARKET REGISTRATION           
002900     03 KVBASLMD             PIC S9(7)           COMP-3.                  
003000*                                 BASIC STOCK DISTRICT QUANTITY           
003100     03 KVBASLKIT            PIC S9(7)           COMP-3.                  
003200*                                 DEALERKIT QUANTITY                      
003300     03 KVBASLM              PIC S9(7)           COMP-3.                  
003400*                                 BASIC STOCK PER MARKET                  
003500     03 TIFINLV              PIC S9(5)           COMP-3.                  
003600*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
003700     03 KDDEALER             PIC X.                                       
003800*                                 BASIC STOCK DEALER CODE                 
003900     03 TEARTNOT             PIC X(40).                                   
004000*                                 PART REMARKS NOTE                       
004100     03 TEARTNOT-BASL        PIC X(40).                                   
004200*                                 PART REMARKS NOTE                       
004300     03 IDARTNUMMER-M.                                                    
004400        05 IDARTNR-MOTSV     PIC S9(9)           COMP-3.                  
004500*                                 PART NUMBER                             
004600        05 M-STRECK          PIC X.                                       
004700        05 REKS-MOTSV        PIC 9.                                       
004800*                                 PART NO CHECK DIGIT                     
004900     03 PRARTBTO-EXP-CP      PIC S9(7)V9(2)      COMP-3.                  
005000*                                 GROSS SALES PRICE (SEK)                 
005100*** END COPY W1150601    LENGTH=214                                       
