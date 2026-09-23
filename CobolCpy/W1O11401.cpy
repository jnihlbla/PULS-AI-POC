000100 01  MOD-W1O11401.                                                        
000200*                                 MOD-COPYTEXT PGM W10114                 
000300*                                 PARTS   REGISTRATION                    
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-KDBASLM-ENTER    PIC X(6).                                    
001300*                                 BASLAGERMARKNAD                         
001400     03 MOD-KDBASLM-PF8      PIC X(6).                                    
001500*                                 BASLAGERMARKNAD                         
001600     03 MOD-IDAO-KEY         PIC X(10).                                   
001700*                                 ÄNDRINGSORDERNUMMER                     
001800     03 MOD-IDPROJ-KEY       PIC X(4).                                    
001900*                                 PARTS PROJEKTIDENTITET                  
002000     03 MOD-CURSOR.                                                       
002100*                                 CURSOR PLACERING                        
002200        05 MOD-CURSOR-RAD    PIC S9(4)           COMP.                    
002300*                                 CURSORPLACERING RAD                     
002400        05 MOD-CURSOR-KOL    PIC S9(4)           COMP.                    
002500*                                 CURSORPLACERING KOLUMN                  
002600     03 MOD-IDARTNR-NEW-ATTR PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-IDARTNR-NEW      PIC X(9).                                    
002900*                                 ARTIKELNUMMER                           
003000     03 MOD-BEART-SVE        PIC X(25).                                   
003100*                                 ARTIKELBENÄMNING                        
003200     03 MOD-TIBASL           PIC 9(6).                                    
003300*                                 BASLAGER MARKNADS KNYTTID               
003400     03 MOD-KDPRODSL         PIC 9(2).                                    
003500*                                 PRODUKTSLAG                             
003600     03 MOD-IDARTNR-ERS      PIC X(9).                                    
003700*                                 ARTIKELNUMMER                           
003800     03 MOD-FLBASL-ATTR      PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-FLBASL           PIC X.                                       
004100*                                 BASLAGER MARKNADSKÖ FLAGGA              
004200     03 MOD-FLATERPAKOE-ATTR PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-FLATERPAKOE      PIC X.                                       
004500     03 MOD-IDFKNGRP         PIC 9(4).                                    
004600*                                 FUNKTIONSGRUPP                          
004700     03 MOD-IDPROJ           PIC X(4).                                    
004800*                                 PARTS PROJEKTIDENTITET                  
004900     03 MOD-KDERS            PIC 9(2).                                    
005000*                                 ERSÄTTNINGSKOD                          
005100     03 MOD-FLBASL-IN-ATTR   PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-FLBASL-IN        PIC X.                                       
005400*                                 BASLAGER MARKNADSKÖ FLAGGA              
005500     03 MOD-FLATERPAKOE-IN-ATTR                                           
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-FLATERPAKOE-IN   PIC X.                                       
005900     03 MOD-RAD              OCCURS 9 TIMES.                              
006000        05 MOD-COL           OCCURS 4 TIMES.                              
006100           07 MOD-KDBASLM    PIC X(6).                                    
006200*                                 BASLAGERMARKNAD                         
006300           07 MOD-AFFECT-ATTR                                             
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600           07 MOD-AFFECT     PIC X.                                       
006700           07 MOD-KVBASLM    PIC 9(7).                                    
006800*                                 BASLAGER TOTAL PER MARKNAD              
006900     03 MOD-TEARTNOT-VAR-ATTR                                             
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-TEARTNOT-VAR     PIC X(40).                                   
007300*                                 ARTIKEL NOTERING                        
007400     03 MOD-IDARTNR-MOTSV-UT-ATTR                                         
007500                             PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-IDARTNR-MOTSV-UT PIC X(9).                                    
007800*                                 ARTIKELNUMMER                           
007900     03 MOD-TEARTNOT-BASL-ATTR                                            
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-TEARTNOT-BASL    PIC X(40).                                   
008300*                                 ARTIKEL NOTERING                        
008400     03 MOD-IDARTNR-MOTSV-IN-ATTR                                         
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-IDARTNR-MOTSV-IN PIC X(2).                                    
008800*                                 MFS BEHANDLING AV INPUTFÄLT             
008900     03 MOD-TEMFSINF         PIC X(61).                                   
009000*                                 INFORMATIONSMEDDELANDE                  
009100*** END COPY W1O11401C0  LENGTH=896                                       
