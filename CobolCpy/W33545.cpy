000100 01  W33545.                                                              
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 RECORD TYPE                             
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 PART NUMBER                             
000600     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
000700*                                 FUNCTION GROUP                          
000800     03 KDSRA                PIC S9(3)           COMP-3.                  
000900*                                 SRA CODE                                
001000     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
001100     03 KDARTURS             PIC X(2).                                    
001200*                                 COUNTRY OF ORIGIN                       
001300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001400*                                 PRODUCT GROUP                           
001500     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
001600*                                 PART NET VOLUME    (CM3)                
001700     03 VKART                PIC S9(7)           COMP-3.                  
001800*                                 PART WEIGHT (G)                         
001900     03 IDSTATNR             PIC S9(9)           COMP-3.                  
002000*                                 STATISTICAL NO.                         
002100     03 KDVSOP               PIC S9(3)           COMP-3.                  
002200*                                 VSOP-CODE                               
002300     03 KDSORT               PIC X(2).                                    
002400*                                 UNIT OF MEASURE                         
002500     03 KDERS                PIC S9(3)           COMP-3.                  
002600*                                 SUPERSESSION CODE                       
002700     03 TIERSDAT             PIC S9(5)           COMP-3.                  
002800*                                 DATE OF SUPERSESSION (YYWWD)            
002900     03 KDBPSR               PIC S9              COMP-3.                  
003000*                                 BASIC PART STOCK RECOMMENDATION         
003100     03 IDLEVNR              PIC X(5).                                    
003200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003300     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
003400*                                 COST OF SALES                           
003500     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003600*                                 STANDARD PRICE                          
003700     03 FLIART               PIC X.                                       
003800*                                 PART IN KIT                             
003900     03 FLLSRDEL             PIC X.                                       
004000     03 IDPROJ               PIC X(4).                                    
004100*                                 PARTS PROJECT IDENTITY                  
004200     03 IDAO                 OCCURS 2 TIMES                               
004300                             PIC X(10).                                   
004400*                                 DESIGN CHANGE NOTICE                    
004500     03 TIFINLV              PIC S9(5)           COMP-3.                  
004600*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
004700     03 IDANSK               PIC S9(3)           COMP-3.                  
004800*                                 PROCURER NO.                            
004900     03 KDVVKL               PIC S9              COMP-3.                  
005000*                                 VOLUME VALUE CLASS                      
005100     03 KDTIPPR              PIC S9              COMP-3.                  
005200*                                 ESTIMATED PRICE CODE                    
005300     03 IDINK                PIC X(4).                                    
005400*                                 PURCHASE IDENTIFICATION NUMBER          
005500     03 KDUART               PIC X.                                       
005600*                                 EXECPTION PARTS                         
005700     03 IDARTNR-MOTSV        PIC S9(9)           COMP-3.                  
005800*                                 CORRESPONDING PART NO                   
005900     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
006000*                                 PURCHASE PRICE                          
006100     03 IDRITN               PIC X(10).                                   
006200*                                 DRAWING NUMBER                          
006300     03 PRDIRLON             PIC S9(4)V9(3)      COMP-3.                  
006400*                                 SURCHARGE COSTS                         
006500     03 PRDMTRL              PIC S9(6)V9(3)      COMP-3.                  
006600*                                 SURCHARGE PACKING MATERIAL              
006700     03 PROVRPAL             PIC S9(4)V9(3)      COMP-3.                  
006800*                                 REMAINING OVERHEAD SURCHARGE            
006900     03 TEORSAK              PIC X(50).                                   
007000*                                 REASON INFORMATION                      
007100     03 TEARTNOT-3           PIC X(40).                                   
007200*                                 PART REMARKS NOTE                       
007300     03 BEART                OCCURS 3 TIMES                               
007400                             PIC X(25).                                   
007500*                                 PART DESCRIPTION                        
007600     03 KDAGE                PIC X.                                       
007700*                                 AGE-CODE                                
007800     03 KDPSLLOC             PIC 9(2).                                    
007900*                                 PRODUCT GROUP LOCAL                     
008000     03 IDKAT                OCCURS 3 TIMES                               
008100                             PIC X(5).                                    
008200     03 IDPROJUP             PIC X(8).                                    
008300*                                 PROJECT ASSIGNMENT                      
008400     03 FLGEMFMC             PIC X.                                       
008500*                                 COMMON FMC/MPNR PARTNO.                 
008600     03 TEARTNOT-7           PIC X(40).                                   
008700*                                 PART REMARKS NOTE                       
008800     03 TIURPROD             PIC S9(5)           COMP-3.                  
008900*                                 OUT OF PRODUCTION DATE (YYWW)           
009000*** END OF VILMAII-COPY LENGTH= 365 BYTES                                 
