000100 01  RESP-WL0134O1.                                                       
000200*                                 RESPONSE FROM PGM WL0134                
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDPRC-KEY.                                                   
000600*                                 PRODUKTIONSKANAL                        
000700        05 RESP-IDPRCBAS     PIC X(3).                                    
000800*                                 PRC-BAS                                 
000900        05 RESP-IDPRCVAR     PIC X.                                       
001000*                                 PRC-VARIANT                             
001100     03 RESP-IDTRP-KEY.                                                   
001200*                                 TRANSPORTIDENTITET                      
001300        05 RESP-IDTRPLOS     PIC X(3).                                    
001400*                                 TRANSPORTLÖSNING                        
001500        05 RESP-IDTRPVAR     PIC X(2).                                    
001600*                                 TRANSPORTLÖSNINGSGRUPP                  
001700     03 RESP-IDANSTNR        PIC Z(5).                                    
001800*                                 ANSTÄLLNINGSNUMMER                      
001900     03 RESP-IDBORD          PIC X(3).                                    
002000*                                 PACK-BORD                               
002100     03 RESP-IDLIST          PIC X(10).                                   
002200*                                 LISTIDENTITET                           
002300     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
002400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
002500*                                 EDAN.                                   
002600     03 RESP-TABELLRAD       OCCURS 1 TO 1100 TIMES                       
002700                             DEPENDING ON RESP-KVRADER-MAX1.              
002800*                                 GRUPP MED TABELL RADER                  
002900        05 RESP-FLORDDEL     PIC X.                                       
003000*                                 ALLMÄN FLAGGA                           
003100        05 RESP-IDPRC-RAD.                                                
003200*                                 PRODUKTIONSKANAL                        
003300           07 RESP-IDPRCBAS  PIC X(3).                                    
003400*                                 PRC-BAS                                 
003500           07 RESP-IDPRCVAR  PIC X.                                       
003600*                                 PRC-VARIANT                             
003700        05 RESP-IDDISTR      PIC Z(3)9.                                   
003800*                                 DISTRIKTNUMMER                          
003900        05 RESP-KDCROSS      PIC X(2).                                    
004000*                                 KOD KROSSDOCK LDC/CDC                   
004100        05 RESP-IDDEPT       PIC Z(2).                                    
004200*                                 AVDELNING I VERKSTAD                    
004300        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
004400*                                 KUNDNUMMER                              
004500        05 RESP-IDORDNR5     PIC X(5).                                    
004600*                                 ORDERNUMMER                             
004700        05 RESP-KDORDKL      PIC X.                                       
004800*                                 ORDERKLASS                              
004900        05 RESP-DALSTORD     PIC 9(6).                                    
005000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005100        05 RESP-TILSTORD     PIC 9(4).                                    
005200*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005300        05 RESP-SUPTID       PIC Z(2)9.9(2).                              
005400*                                 TOTAL PRODUKTIONSTID TIM+MIN            
005500        05 RESP-TIRFSDAT     PIC 9(6).                                    
005600*                                 KLART FÖR TRANSPORT ÅÅMMDD              
005700        05 RESP-TIRFSTID     PIC 9(4).                                    
005800*                                 KLART FÖR TRANSPORT (TTMM)              
005900        05 RESP-KVORDRAD     PIC Z(4)9.                                   
006000*                                 ANTAL ORDERRADER                        
006100        05 RESP-VKORDNTO     PIC Z(5)9.9.                                 
006200*                                 ORDERVIKT NETTO (KG)                    
006300        05 RESP-VLORDNTO     PIC Z(3)9.9(3).                              
006400*                                 ORDERVOLYM NETTO (M3)                   
006500        05 RESP-IDTRP.                                                    
006600*                                 TRANSPORTIDENTITET                      
006700           07 RESP-IDTRPLOS  PIC X(3).                                    
006800*                                 TRANSPORTLÖSNING                        
006900           07 RESP-IDTRPVAR  PIC X(2).                                    
007000*                                 TRANSPORTLÖSNINGSGRUPP                  
007100        05 RESP-KVPLOCK      PIC Z(5)9.                                   
007200*                                 ANTAL                                   
007300        05 RESP-IDPRODNR     PIC 9(7).                                    
007400*                                 PRODUKTIONSNUMMER                       
007500        05 RESP-IDPLKLST     PIC 9(3).                                    
007600*                                 PLOCKLISTNUMMER                         
007700*** END OF VILMAII-COPY LENGTH= 102334 BYTES                              
