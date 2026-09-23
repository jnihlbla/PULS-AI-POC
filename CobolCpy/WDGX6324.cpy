000100 01  6324-WDGX6324.                                                       
000200*                                 SKROTFÖRSLAG                            
000300*                                 ARTIKLAR                                
000400*                                 FYSISK NYCKEL: KY6324                   
000500*                                 (IDARTNR + IDDC + KDSTASKR)             
000600     03 6324-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 6324-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 6324-KDSTASKR        PIC S9              COMP-3.                  
001300*                                 STATUS FÖR SKROTAD ARTIKEL              
001400*                                 STATUS SCRAPED PART                     
001500     03 6324-FLSKROT-GODK    PIC X.                                       
001600*                                 SKROTNING BEORDRAD                      
001700*                                 SCRAPPING ORDERED BY PROCURER           
001800     03 6324-BELAGINS-DEL    PIC X(60).                                   
001900*                                 DEL AV LAGERINSTRUKTION                 
002000*                                 PART OF WAREHOUSE INSTRUCTIONS          
002100     03 6324-IDANALYS        PIC X(12).                                   
002200*                                 ANALYSNUMMER                            
002300*                                 ANALYSIS NUMBER                         
002400     03 6324-IDDISTR         PIC S9(5)           COMP-3.                  
002500*                                 DISTRIKTNUMMER                          
002600*                                 DISTRICT NUMBER                         
002700     03 6324-IDKUNDNR        PIC S9(7)           COMP-3.                  
002800*                                 KUNDNUMMER                              
002900*                                 CUSTOMER NO                             
003000     03 6324-IDKONTO         PIC S9(11)          COMP-3.                  
003100*                                 KONTO                                   
003200*                                 ACCOUNT                                 
003300     03 6324-IDKST           PIC X(10).                                   
003400*                                 KOSTNADSSTÄLLE                          
003500*                                 COST CENTRE                             
003600     03 6324-IDPERSON        PIC S9(3)           COMP-3.                  
003700*                                 PERSONKOD                               
003800*                                 STAFF CODE                              
003900     03 6324-IDUSER          PIC X(8).                                    
004000*                                 ANVÄNDARENS SÄKERHETS ID                
004100*                                 USER SECURITY-IDENTITY                  
004200     03 6324-KDFRAKT         PIC S9(3)           COMP-3.                  
004300*                                 FRAKTSÄTT DC TILL KUND                  
004400*                                 FREIGHT CODE                            
004500     03 6324-KDORDKL         PIC S9              COMP-3.                  
004600*                                 ORDERKLASS                              
004700*                                 ORDER CLASS                             
004800     03 6324-KVSKROT-BEORD   PIC S9(7)           COMP-3.                  
004900*                                 ANTAL SENASTE SKROTORDER                
005000*                                 QUANTITY LAST SCRAPPINGORDER            
005100     03 6324-KVSKROT-KVAR    PIC S9(7)           COMP-3.                  
005200*                                 KVARLIGGANDE ANTAL                      
005300     03 6324-FLJUSTBUFF      PIC X.                                       
005400*                                 JUSTERA BUFFERT                         
005500*                                 ADJUST BUFFER QUANTITY                  
005600     03 6324-BEANST          PIC X(25).                                   
005700*                                 ANSTÄLLDS NAMN                          
005800*                                 NAME OF EMPLOYED                        
005900     03 6324-KVSKROT-ONDEM   PIC S9(7)           COMP-3.                  
006000*                                 SKROTANTAL TILL ONDEMAND                
006100*                                 SCRAP QTY  TO  ONDEMAND                 
006200     03 6324-KDERS-UTG       PIC S9(3)           COMP-3.                  
006300*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
006400*                                 OBSOLETION SUPERSESSION CODE            
006500     03 6324-KVTILLG-CDC     PIC S9(7)           COMP-3.                  
006600*                                 LAGERTILLGÅNG-CDC                       
006700     03 6324-KVTILLG-SDC     PIC S9(7)           COMP-3.                  
006800*                                 LAGERTILLGÅNG-SDC                       
006900     03 6324-KVAKS-CDC       PIC S9(7)           COMP-3.                  
007000*                                 DEL AV AK SOM LIGGER I CDC              
007100*                                 PART OF AK IN THE CDC                   
007200     03 6324-KVAKS-SDC       PIC S9(7)           COMP-3.                  
007300*                                 DEL AV AK SOM LIGGER I SDC              
007400*                                 PART OF AK IN THE SDC                   
007500     03 6324-BEEMBLEM        OCCURS 20 TIMES                              
007600                             PIC X(5).                                    
007700*                                 EMBLEM                                  
007800*                                 EMBLEM                                  
007900     03 6324-SUTPO-TOT       PIC S9(7)           COMP-3.                  
008000*                                 TPO-KVANTITET, TOTAL                    
008100*                                 TPO-QUANTITY, TOTAL                     
008200     03 6324-FILLER          PIC X(3).                                    
008300*** END OF VILMAII-COPY LENGTH= 280 BYTES                                 
