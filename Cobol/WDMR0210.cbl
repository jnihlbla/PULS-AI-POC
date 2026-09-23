000100 Id Division.                                                             
000200     skip2                                                                
000300 Program-Id.     wdmr0210.                                                
000400*Author          ODD OLSEN.                                               
000500*Date-Written.   91/12/01.                                                
000600*                                                                         
000700* Programmet användes för att hitta det n:te ordet i en                   
000800* sträng på max 200 tkn.                                                  
000900*                                                                         
001000* Programmet retunerar positionen samt längden på det                     
001100* n:te ordet.                                                             
001200*                                                                         
001300* Om det inte finns något n:te ord eller om strängen är blank             
001400* så returneras 0 som position.                                           
001500                                                                          
001600 Data Division.                                                           
001700                                                                          
001800 Working-Storage Section.                                                 
001900                                                                          
001901                                                                          
001910*    -- CHECKED BY WY2000                                                 
002000 77  yes          Pic X Value 'Y'.                                        
002100 77  nej          Pic X Value 'N'.                                        
002200 77  leave        Pic X Value 'N'.                                        
002500 77  del          Pic X Value Space.                                      
002600 77  sub-del      Pic X Value ''''.                                       
002700 77  w-word-string Pic X(200).                                            
002800 77  max-str       Pic 9(4) Comp Value 100.                               
002900 77  ix            Pic 9(4) Comp.                                         
003000 77  w-word-position Pic 9(4) Comp.                                       
003100 77  ord-counter Pic 9(4) Comp.                                           
003200                                                                          
003300 Linkage Section.                                                         
003400                                                                          
003500 01  lnk-area.                                                            
003600     03 word-number         Pic 9(4) Comp.                                
003700     03 word-type           Pic 9(4) Comp.                                
003710     03 start-pos           Pic 9(4) Comp.                                
003720     03 end-pos             Pic 9(4) Comp.                                
003801     03 word-sub-del        Pic x.                                        
003810     03 word-string         Pic x(200).                                   
003900     03 word-position       Pic 9(4) Comp.                                
004000     03 word-length         Pic 9(4) Comp.                                
004100     03 leading-space       Pic 9(4) Comp.                                
004200     eject                                                                
004300 Procedure Division Using lnk-area.                                       
004400                                                                          
004500 STYR Section.                                                            
004600                                                                          
004700* Init                                                                    
005000     If word-string = Space                                               
005100       Goback                                                             
005200     End-If                                                               
005300                                                                          
005302     Move word-sub-del to sub-del                                         
005310     Move nej to leave                                                    
005320     Move Zero To word-position word-length leading-space                 
005400     Move word-string To w-word-string                                    
005410                                                                          
005411     Perform WORD-SEARCH                                                  
005412                                                                          
005413     Goback                                                               
005414     .                                                                    
012510     eject                                                                
030400 WORD-SEARCH Section.                                                     
030500                                                                          
030600     Move word-string To w-word-string                                    
030700                                                                          
030800* Hitta ord                                                               
030900     If word-number = zero                                                
031000       Move ord-counter to word-number                                    
031100       Add 1 to word-number                                               
031200     Else                                                                 
031300       Move start-pos to ix                                               
031400       Move 0 to ord-counter                                              
031500     End-If                                                               
031600                                                                          
031700* Avsluta om resten av raden är space                                     
031800     If w-word-string(ix:) = space                                        
031900       Goback                                                             
032000     End-If                                                               
032100                                                                          
032200* Läs till första tecken                                                  
032300                                                                          
032400     Move Zero to leading-space                                           
032500     Perform Until Not w-word-string(ix:1) = Space                        
032600       Add +1 To ix leading-space                                         
032700     End-Perform                                                          
032800                                                                          
032900     Move ix To w-word-position                                           
033000     Move Space to del                                                    
033100                                                                          
033200     If word-type < 0 And w-word-string(ix:1) = sub-del                   
033300       Move sub-del to del                                                
033400       Add 1 to ix                                                        
033500     End-If                                                               
033600                                                                          
033700     Perform Until ix > end-pos or leave = yes                            
033800                                                                          
033900       If w-word-string(ix:1) = del                                       
034000         Add 1 to ord-counter                                             
034010                                                                          
034100         If ord-counter = word-number                                     
034200           Move w-word-position to word-position                          
034300           Compute word-length = ix - word-position                       
034400           If word-type > 0 And del = sub-del                             
034500             Add +1 To word-length ix                                     
034600           End-if                                                         
034700           Move yes to leave                                              
034800         Else                                                             
034900           If word-type > 0 And del = sub-del                             
035000             Move 0 to leading-space                                      
035100           Else                                                           
035200             Move 1 to leading-space                                      
035300           End-If                                                         
035400                                                                          
035500           Move Space to del                                              
035600                                                                          
035700           Add +1 To ix                                                   
035800           Perform Until (ix > end-pos) Or                                
035900                     Not (w-word-string(ix:1) = Space)                    
036000             Add +1 To ix leading-space                                   
036100           End-Perform                                                    
036200           Move ix To w-word-position                                     
036210                                                                          
036300           If word-type > 0 And ix < end-pos                              
036400             If w-word-string(ix:1) = sub-del                             
036500               Move sub-del to del                                        
036600               Add +1 to ix                                               
036700             End-If                                                       
036800           End-If                                                         
036810                                                                          
036900         End-If                                                           
037000       Else                                                               
037100         Add +1 To ix                                                     
037101                                                                          
037110         If word-type > 1 And ix < end-pos                                
037120           If w-word-string(ix:1) = sub-del                               
037130             Move sub-del to del                                          
037140             Add +1 to ix                                                 
037150           End-If                                                         
037160         End-If                                                           
037170                                                                          
037200       End-If                                                             
037300     End-Perform                                                          
037400     .                                                                    
