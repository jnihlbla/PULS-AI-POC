000100 01  MOD-W2O40101.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDARTNR-IN       PIC X(9).                                    
000700*                                 PART NUMBER                             
000800     03 MOD-BLADDRING-ANT    PIC X(3).                                    
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 PART NUMBER                             
001300     03 MOD-STRECK           PIC X.                                       
001400     03 MOD-REKSIFFR         PIC 9.                                       
001500*                                 PART NO CHECK DIGIT                     
001600     03 MOD-BEART-SVE        PIC X(25).                                   
001700*                                 PART DESCRIPTION                        
001800     03 MOD-IDDC-UT          PIC X(2).                                    
001900*                                 WAREHOUSE IDENTIFIER                    
002000     03 MOD-AREA.                                                         
002100        05 MOD-IDANSK        PIC Z(2)9.                                   
002200*                                 PROCURER NO.                            
002300        05 MOD-IDLEVNR-SHIP  PIC X(5).                                    
002400*                                 SHIPPING SUPPLIER                       
002500        05 MOD-IDFKNGRP      PIC Z(3)9.                                   
002600*                                 FUNCTION GROUP                          
002700        05 MOD-IDBERED       PIC Z9.                                      
002800        05 MOD-KVAARLF       PIC Z9.                                      
002900*                                 NO OF YEARS AFTER EOP                   
003000        05 MOD-IDPLANGR-AG   PIC 9.                                       
003100        05 MOD-IDLEVNR       PIC X(5).                                    
003200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003300        05 MOD-IDPROJ        PIC X(4).                                    
003400*                                 PARTS PROJECT IDENTITY                  
003500        05 MOD-IDINK         PIC X(4).                                    
003600*                                 PURCHASE IDENTIFICATION NUMBER          
003700        05 MOD-DAPUBLW       PIC Z(4)9.                                   
003800*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
003900        05 MOD-KDSORT        PIC X(2).                                    
004000*                                 UNIT OF MEASURE                         
004100        05 MOD-KDAVT         PIC 9.                                       
004200*                                 AGREEMENT CODE                          
004300        05 MOD-KVSLAGER      PIC Z(5)9.                                   
004400*                                 SAFETY STOCK                            
004500        05 MOD-TIMANSEC      PIC 9(6).                                    
004600*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
004700        05 MOD-DALPUBLW      PIC 9(5).                                    
004800*                                 YEAR - WEEK - DAY  (YYWWD)              
004900        05 MOD-KDERS-UTG     PIC Z9.                                      
005000*                                 OBSOLETION SUPERSESSION CODE            
005100        05 MOD-KDARTURS      PIC X(2).                                    
005200*                                 COUNTRY OF ORIGIN                       
005300        05 MOD-KVEOQ         PIC Z(6)9.                                   
005400        05 MOD-TIURPROD      PIC 9(4).                                    
005500*                                 OUT OF PRODUCTION DATE (YYWW)           
005600        05 MOD-FLLSRDEL      PIC X.                                       
005700        05 MOD-FLJIT         PIC X.                                       
005800*                                 JUST-IN-TIME FLAG                       
005900        05 MOD-KVREFBER      PIC Z(6)9.                                   
006000*                                 CALCULATED REFILLING QUANTITY           
006100        05 MOD-TIREFPAF      PIC 9(6).                                    
006200*                                 DATE MANUAL REFILLING QTY               
006300        05 MOD-KVVECKOR-LT   PIC Z9.                                      
006400        05 MOD-TIMANLED      PIC 9(6).                                    
006500*                                 END DATE MAN.LEAD TIME (YYMMDD)         
006600        05 MOD-KDFARLIG      PIC 9.                                       
006700*                                 DANGEROUS GOODS CODE                    
006800        05 MOD-FLWILSON      PIC X.                                       
006900*                                 FLAG TO USE WILSON OR NOT               
007000        05 MOD-KVPALL        PIC Z(6)9.                                   
007100*                                 QUANTITY IN PALLET                      
007200        05 MOD-KVDAGAR-INL   PIC Z9.                                      
007300        05 MOD-KDPRISKL      PIC X.                                       
007400*                                 PRICE CLASS                             
007500        05 MOD-KDFREKKL      PIC X.                                       
007600*                                 FREQ. CLASS                             
007700        05 MOD-IDREFTAB      PIC X.                                       
007800*                                 REFILLINGTABLE IDENTIFIER               
007900        05 MOD-KVULOAD       PIC Z(6)9.                                   
008000*                                 MIN LOAD FROM SUPPLIER                  
008100        05 MOD-KVDAGAR-TT    PIC Z9.                                      
008200        05 MOD-TIREFSTO      PIC 9(6).                                    
008300*                                 STOPPED FOR ORDERING UNTIL              
008400        05 MOD-IDKAT-1       PIC X(5).                                    
008500        05 MOD-IDKAT-2       PIC X(5).                                    
008600        05 MOD-IDKAT-3       PIC X(5).                                    
008700        05 MOD-IDPROENH      OCCURS 3 TIMES                               
008800                             INDEXED MOD-IY                               
008900                             PIC X(8).                                    
009000*                                 PRODUCTION UNIT                         
009100        05 MOD-IDKAT         OCCURS 11 TIMES                              
009200                             INDEXED MOD-IZ                               
009300                             PIC X(5).                                    
009400        05 MOD-IDAO          OCCURS 5 TIMES                               
009500                             INDEXED MOD-IX                               
009600                             PIC X(10).                                   
009700*                                 DESIGN CHANGE NOTICE                    
009800        05 MOD-VARNOT        PIC X(40).                                   
009900*                                 PART REMARKS NOTE                       
010000        05 MOD-PLANOT        PIC X(40).                                   
010100*                                 PART REMARKS NOTE                       
010200        05 MOD-TEARTNOT      OCCURS 2 TIMES                               
010300                             PIC X(40).                                   
010400*                                 PART REMARKS NOTE                       
010500     03 MOD-TEMFSINF         PIC X(55).                                   
010600*                                 INFORMATION MESSAGE                     
010700*** END OF VILMAII-COPY LENGTH= 577 BYTES                                 
