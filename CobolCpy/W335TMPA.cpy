000100 01  ARTINFO.                                                             
000200*                                 PARTNO. INFORM.  TRANS TO               
000300*                                 MARKET COMPANY                          
000400*                                 IDPTYP =401                             
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 IDVTYP               PIC X.                                       
000800*                                 RECORD TYPE VERSION                     
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 PART NUMBER                             
001100     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001200*                                 FUNCTION GROUP                          
001300     03 KDSRA                PIC S9(3)           COMP-3.                  
001400*                                 SRA CODE                                
001500     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
001600*                                 QUANTITY IN BULK PACK Q1                
001700     03 KDARTURS             PIC S9(3)           COMP-3.                  
001800*                                 COUNTRY OF ORIGIN                       
001900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002000*                                 PRODUCT GROUP                           
002100     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
002200*                                 PART NET VOLUME    (CM3)                
002300     03 VKART                PIC S9(7)           COMP-3.                  
002400*                                 PART WEIGHT (G)                         
002500     03 KDVSOP               PIC S9(3)           COMP-3.                  
002600*                                 VSOP-CODE                               
002700     03 IDSTATNR             PIC S9(9)           COMP-3.                  
002800*                                 STATISTICAL NO.                         
002900     03 KDSORT               PIC X(2).                                    
003000*                                 UNIT OF MEASURE                         
003100     03 KDERS                PIC S9(3)           COMP-3.                  
003200*                                 SUPERSESSION CODE                       
003300     03 KDBPSR               PIC S9              COMP-3.                  
003400*                                 BASIC PART STOCK RECOMMENDATION         
003500     03 KDBBCL               PIC 9.                                       
003600*                                 RETURNABLE PART                         
003700     03 IDLEVNR              PIC S9(5)           COMP-3.                  
003800*                                 SUPPLIER NUMBER                         
003900     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
004000*                                 COST OF SALES                           
004100     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
004200*                                 STANDARD PRICE                          
004300     03 FLIART               PIC X.                                       
004400     03 IDPROJ               PIC X(4).                                    
004500*                                 PARTS PROJECT IDENTITY                  
004600     03 IDAO                 OCCURS 2 TIMES                               
004700                             PIC X(10).                                   
004800*                                 DESIGN CHANGE NOTICE                    
004900     03 TIFINLEV             PIC S9(7)           COMP-3.                  
005000*                                 DATE 1:ST GOODS REC (YYMMDD)            
005100     03 IDANSK               PIC S9(3)           COMP-3.                  
005200*                                 PROCURER NO.                            
005300     03 KVPB                 PIC S9(6)V9(1)      COMP-3.                  
005400*                                 PERIOD REQUIREMENTS                     
005500     03 KDVVKL               PIC S9              COMP-3.                  
005600*                                 VOLUME VALUE CLASS                      
005700     03 BELEVART             PIC X(30).                                   
005800*                                 SUPPLIERS PART DESCRIPTION              
005900     03 KDTIPPR              PIC S9              COMP-3.                  
006000*                                 ESTIMATED PRICE CODE                    
006100     03 IDINK                PIC S9(3)           COMP-3.                  
006200*                                 PURCHASE IDENTIFICATION NUMBER          
006300     03 TIREGDAT             PIC S9(7)           COMP-3.                  
006400*                                 REGISTRATION DATE (YYMMDD)              
006500     03 KDUART               PIC X.                                       
006600*                                 EXECPTION PARTS                         
006700     03 IDARTNR-MOTSV        PIC S9(9)           COMP-3.                  
006800*                                 CORRESPONDING PART NO                   
006900     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
007000*                                 PURCHASE PRICE                          
007100     03 IDRITN               PIC X(10).                                   
007200*                                 DRAWING NUMBER                          
007300     03 PRHANTK              PIC S9(5)V9(2)      COMP-3.                  
007400*                                 SURCHARGE COSTS +                       
007500*                                 CALCULATED COST +                       
007600*                                 OVR PAL                                 
007700     03 KDAGE                PIC X.                                       
007800*                                 AGE-CODE                                
007810*                                 AGE-CODE                                
007820     03 KDPSLLOC             PIC 9(2).                                    
007830*                                 PRODUKTSLAG LOKALT                      
007840*                                 PRODUCT GROUP LOCAL                     
007850     03 SLAG-IDLEVNR         PIC S9(5)           COMP-3.                  
007860*                                 LEVERANTÖRNUMMER                        
007870*                                 SUPPLIER NUMBER (VENDORNUMBER)          
007880*** END OF VILMAII-COPY LENGTH= 160 BYTES                                 
