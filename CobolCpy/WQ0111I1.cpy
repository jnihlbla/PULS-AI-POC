000100 01  REQU-WQ0111I1.                                                       
000200*                                 MID-COPYTEXT FOR WQ011100               
000300     03 REQU-KDREPLVL        PIC X.                                       
000400*                                 RAPPORTNIVÅ - D(ETALJ)/S(SUMMA)         
000500*                                 REPORT LEVEL - D(ETAIL)/S(SUM)          
000600     03 REQU-KDQOUT          PIC X.                                       
000700*                                 TYP AV OUTPUT PÅ WEBBFRÅGA              
000800*                                 OUTPUT TYPE FOR WEB QUERY               
000900     03 REQU-IDMAIL          PIC X(60).                                   
001000*                                 MAIL ADRESS                             
001100*                                 MAIL ADDRESS                            
001200     03 REQU-SEARCH-TABLE    OCCURS 15 TIMES.                             
001300*                                 DATA ELEMENTS WITH SELECTION            
001400*                                 CRITERIA                                
001500        05 REQU-IDELMT-SEARCH                                             
001600                             PIC X(16).                                   
001700*                                 DATAELEMENTIDENTITET                    
001800*                                 DATA ITEM NAME                          
001900        05 REQU-TEELMTVAL-SEARCH                                          
002000                             PIC X(1000).                                 
002100*                                 LISTA AV VÄRDEN                         
002200*                                 LIST OF VALUES                          
002300     03 REQU-ORDER-BY-TABLE  OCCURS 15 TIMES.                             
002400*                                 DATA ELEMENTS SPECIFYING                
002500*                                 SORT ORDER                              
002600        05 REQU-IDELMT-OB    PIC X(16).                                   
002700*                                 DATAELEMENTIDENTITET                    
002800*                                 DATA ITEM NAME                          
002900     03 REQU-DATA-COLUMNS-TABLE                                           
003000                             OCCURS 15 TIMES.                             
003100*                                 COLUMNS TO BE INCLUDED IN               
003200*                                 THE REPORT/EXTRACT                      
003300        05 REQU-TESELCOL     PIC X(50).                                   
003400*                                 COLUMN I SELECTSATS                     
003500*                                 COLUMN IN SELECT STATEMENT              
003600     03 REQU-TESSVDATA-HEADERS                                            
003700                             PIC X(800).                                  
003800*                                 SEMIKOLON-SEP. DATA (EXCEL FMT)         
003900*                                 SEMICOLON SEP VALUES FOR EXCEL          
004000     03 REQU-KVGRPCOL        PIC 9(2).                                    
004100*                                 ANTAL GROUP-BY KOLUMNER                 
004200*                                 NBR OF GROUP-BY COLUMNS                 
004300     03 REQU-SEARCH-CRIT-TABLE                                            
004400                             OCCURS 10 TIMES.                             
004500*                                 SEARCH CRITERIA                         
004600*                                 (ELEMENT NAME + VALUES)                 
004700        05 REQU-TEELMTVAL-SCRIT                                           
004800                             PIC X(1000).                                 
004900*                                 LISTA AV VÄRDEN                         
005000*                                 LIST OF VALUES                          
005100*** END OF VILMAII-COPY LENGTH= 27094 BYTES                               
