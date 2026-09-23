000100 01  W233LI2X.                                                            
000200     03 IDPTYP               PIC X(3)                                     
000300                             VALUE SPACES.                                
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 IDARTNR              PIC Z(7)9                                    
000700                             VALUE ZEROS.                                 
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 IDDC                 PIC X(2)                                     
001100                             VALUE SPACES.                                
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 IDLEVNR              PIC X(5)                                     
001500                             VALUE SPACES.                                
001600*                                 LEVERANTÖRNUMMER                        
001700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001800     03 TIAVROP-AVS          PIC 9(4)                                     
001900                             VALUE ZEROS.                                 
002000*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002100*                                 (ÅÅVV)                                  
002200     03 TIAVROP-INL          PIC 9(4)                                     
002300                             VALUE ZEROS.                                 
002400*                                 INLEVERANSDATUM (PLANERAD)              
002500*                                 (ÅÅVV)                                  
002600     03 TIAVROP-DISP         PIC 9(4)                                     
002700                             VALUE ZEROS.                                 
002800*                                 DISPONIBELVECKA  (PLANERAD)             
002900*                                 (ÅÅVV)                                  
003000     03 KVAVROP              PIC Z(6)9                                    
003100                             VALUE ZEROS.                                 
003200*                                 AVROPSKVANTITET                         
003300     03 TILEVDAG             PIC 9                                        
003400                             VALUE ZERO.                                  
003500*                                 AVSÄNDNINGSDAG INOM VECKA               
003600*                                 DELIVERY WEEK DAY                       
003700*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
