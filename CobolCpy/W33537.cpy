000100 01  W33537-CTX.                                                          
000200*                                 PARTNO. INFORM.  TRANS TO               
000300*                                 MARKET COMPANY                          
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 IDVTYP               PIC X.                                       
000700*                                 RECORD TYPE VERSION                     
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 PART NUMBER                             
001000     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001100*                                 FUNCTION GROUP                          
001200     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
001300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001400*                                 PRODUCT GROUP                           
001500     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
001600*                                 PART NET VOLUME    (CM3)                
001700     03 VKART                PIC S9(7)           COMP-3.                  
001800*                                 PART WEIGHT (G)                         
001900     03 KDVSOP               PIC S9(3)           COMP-3.                  
002000*                                 VSOP-CODE                               
002100     03 KDSORT               PIC X(2).                                    
002200*                                 UNIT OF MEASURE                         
002300     03 KDBPSR               PIC S9              COMP-3.                  
002400*                                 BASIC PART STOCK RECOMMENDATION         
002500     03 KDBBCL               PIC 9.                                       
002600*                                 RETURNABLE PART                         
002700     03 IDLEVNR              PIC X(5).                                    
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
003000*                                 COST OF SALES                           
003100     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003200*                                 STANDARD PRICE                          
003300     03 FLIART               PIC X.                                       
003400*                                 PART IN KIT                             
003500     03 IDPROJ               PIC X(4).                                    
003600*                                 PARTS PROJECT IDENTITY                  
003700     03 IDAO                 OCCURS 2 TIMES                               
003800                             PIC X(10).                                   
003900*                                 DESIGN CHANGE NOTICE                    
004000     03 TIFINLV              PIC S9(5)           COMP-3.                  
004100*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
004200     03 IDANSK               PIC S9(3)           COMP-3.                  
004300*                                 PROCURER NO.                            
004400     03 KVPB                 PIC S9(6)V9(1)      COMP-3.                  
004500*                                 PERIOD REQUIREMENTS                     
004600     03 KDVVKL               PIC S9              COMP-3.                  
004700*                                 VOLUME VALUE CLASS                      
004800     03 KDTIPPR              PIC S9              COMP-3.                  
004900*                                 ESTIMATED PRICE CODE                    
005000     03 IDINK                PIC X(4).                                    
005100*                                 PURCHASE IDENTIFICATION NUMBER          
005200     03 TIREGDAT             PIC S9(7)           COMP-3.                  
005300*                                 REGISTRATION DATE (YYMMDD)              
005400     03 KDUART               PIC X.                                       
005500*                                 EXECPTION PARTS                         
005600     03 IDARTNR-MOTSV        PIC S9(9)           COMP-3.                  
005700*                                 CORRESPONDING PART NO                   
005800     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
005900*                                 PURCHASE PRICE                          
006000     03 IDRITN               PIC X(10).                                   
006100*                                 DRAWING NUMBER                          
006200     03 PRHANTK              PIC S9(5)V9(2)      COMP-3.                  
006300*                                 SURCHARGE COSTS +                       
006400*                                 CALCULATED COST +                       
006500*                                 OVR PAL                                 
006600     03 KDAGE                PIC X.                                       
006700*                                 AGE-CODE                                
006800     03 TEORSAK              PIC X(50).                                   
006900*                                 REASON INFORMATION                      
007000     03 TEARTNOT             PIC X(40).                                   
007100*                                 PART REMARKS NOTE                       
007200     03 BEART                OCCURS 2 TIMES                               
007300                             PIC X(25).                                   
007400*                                 PART DESCRIPTION                        
007500     03 TIERSDAT             PIC S9(5)           COMP-3.                  
007600*                                 DATE OF SUPERSESSION (YYWWD)            
007700     03 FLLSRDEL             PIC X.                                       
007800     03 KDLTK                PIC S9              COMP-3.                  
007900*                                 STOCK BELONGING CODE                    
008000     03 IDSKYLT              OCCURS 2 TIMES                               
008100                             PIC X(3).                                    
008200*                                 NATIONALITY SIGN                        
008300*                                 LANGUAGE IDENTIFIER                     
008400     03 KDSRA                PIC S9(3)           COMP-3.                  
008500*                                 SRA CODE                                
008600     03 KDARTURS             PIC X(2).                                    
008700*                                 COUNTRY OF ORIGIN                       
008800     03 KDERS                PIC S9(3)           COMP-3.                  
008900*                                 SUPERSESSION CODE                       
009000     03 IDSTATNR             OCCURS 6 TIMES                               
009100                             PIC S9(9)           COMP-3.                  
009200*                                 STATISTICAL NO.                         
009300     03 KDPSLLOC             PIC 9(2).                                    
009400*                                 PRODUCT GROUP LOCAL                     
009500*** END OF VILMAII-COPY LENGTH= 306 BYTES                                 
