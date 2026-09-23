000100 01  W4754A01.                                                            
000200*                                 INVOICE LINE INFORMATION TO             
000300*                                 SWITZERLAND NC                          
000400     03 IDFAKT               PIC S9(7)           COMP-3                   
000500                             VALUE ZEROS.                                 
000600*                                 INVOICE NO.                             
000700     03 IDDISTR              PIC S9(5)           COMP-3                   
000800                             VALUE ZEROS.                                 
000900*                                 DISTRICT NUMBER                         
001000     03 IDKUNDNR             PIC S9(7)           COMP-3                   
001100                             VALUE ZEROS.                                 
001200*                                 CUSTOMER NO                             
001300     03 IDORDNR7             PIC 9(7)                                     
001400                             VALUE ZEROS.                                 
001500*                                 ORDER NUMBER                            
001600     03 IDARTNR              PIC S9(9)           COMP-3                   
001700                             VALUE ZEROS.                                 
001800*                                 PART NUMBER                             
001900     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3                   
002000                             VALUE ZEROS.                                 
002100*                                 PART NET WEIGHT (KG)                    
002200     03 KVLEVART             PIC S9(7)           COMP-3                   
002300                             VALUE ZEROS.                                 
002400*                                 DELIVERED QUANTITY                      
002500     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3                   
002600                             VALUE ZEROS.                                 
002700*                                 NET PRICE EACH   (FOB NET)              
002800     03 IDPRODNR             PIC S9(7)           COMP-3                   
002900                             VALUE ZEROS.                                 
003000*                                 PRODUCTION NUMBER                       
003100     03 TIFAKT               PIC S9(7)           COMP-3                   
003200                             VALUE ZEROS.                                 
003300*                                 INVOICING DATE   (YYMMDD)               
003400     03 TIPACKN              PIC S9(7)           COMP-3                   
003500                             VALUE ZEROS.                                 
003600*                                 PACKING DATE           (YYMMDD)         
003700     03 IDKOLLI              PIC S9(5)           COMP-3                   
003800                             VALUE ZEROS.                                 
003900*                                 CASE NUMBER                             
004000     03 KDARTURS             PIC X(2)                                     
004100                             VALUE SPACES.                                
004200*                                 COUNTRY OF ORIGIN                       
004300     03 FILLER               PIC X                                        
004400                             VALUE SPACE.                                 
004500     03 IDSTATNR             PIC S9(9)           COMP-3                   
004600                             VALUE ZEROS.                                 
004700*                                 STATISTICAL NO.                         
004800     03 VKORDBTO-KOLLI       PIC S9(7)           COMP-3                   
004900                             VALUE ZEROS.                                 
005000*                                 ORDER WEIGHT GROSS PER CASE             
005100     03 VKORDNTO-KOLLI       PIC S9(7)           COMP-3                   
005200                             VALUE ZEROS.                                 
005300*                                 ORDER WEIGHT NET PER CASE               
005400*** END OF VILMAII-COPY LENGTH= 67 BYTES                                  
