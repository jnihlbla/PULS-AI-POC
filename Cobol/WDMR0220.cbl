000100 Id Division.                                                             
000200     skip2                                                                
000300 Program-Id.     WDMR0220.                                                
000400*Author          ODD OLSEN.                                               
000500*Date-Written.   91/12/01.                                                
001500                                                                          
001600 Data Division.                                                           
001700                                                                          
001800 Working-Storage Section.                                                 
001900                                                                          
001901                                                                          
001910*    -- CHECKED BY WY2000                                                 
002000 77  ja              Pic X Value 'Y'.                                     
002100 77  nej             Pic X Value 'N'.                                     
002200 77  leave           Pic X Value 'N'.                                     
002300 77  del             Pic X Value Space.                                   
002400 77  sub-del         Pic X Value ''''.                                    
002700 77  ix              Pic 9(4) Comp.                                       
003000                                                                          
003100 Linkage Section.                                                         
003200                                                                          
003300 01  lnk-area.                                                            
003801     03 parm-del            Pic x.                                        
003802     03 parm-str-del        Pic x.                                        
003810     03 parm-string         Pic x(200).                                   
003820     03 parm-pos            Pic 9(4).                                     
003830     03 parm-length         Pic 9(4) Comp.                                
003850     eject                                                                
003860 Procedure Division Using lnk-area.                                       
003870                                                                          
003880 STYR Section.                                                            
003890                                                                          
003900* Init                                                                    
003910     Move Zero To parm-pos parm-length                                    
003920                                                                          
004400     Move parm-str-del to sub-del                                         
004500     Move nej to leave                                                    
004510                                                                          
005700     Move 1 to ix                                                         
005701                                                                          
005702     If parm-string(ix:1) = Space                                         
005720       Goback                                                             
005730     End-If                                                               
007800                                                                          
007900     Move ix To parm-pos                                                  
008000     Move parm-del to del                                                 
008100                                                                          
008200     If parm-string(ix:1) = sub-del                                       
008300       Move sub-del to del                                                
008400       Add 1 to ix                                                        
008500     End-If                                                               
008600                                                                          
008700     Perform Until ix > 200 Or leave = ja                                 
008800       If parm-string(ix:) = Space                                        
008810         Move ja To leave                                                 
008820       Else                                                               
008900         If parm-string(ix:1) = del                                       
009800           Move ja To leave                                               
012300         Else                                                             
012400           Add 1 To ix                                                    
012600           If ix < 200 And del Not = sub-del                              
012700             If parm-string(ix:1) = sub-del                               
012800               Move sub-del to del                                        
012900               Add 1 to ix                                                
013000             End-If                                                       
013100           End-If                                                         
013300         End-If                                                           
013310       End-If                                                             
013400     End-Perform                                                          
013402     If ix > 200                                                          
013403       Move 200 to ix                                                     
013404     End-if                                                               
013420     Compute parm-length = ix - parm-pos                                  
013430     If del = sub-del                                                     
013440       Add 1 To parm-length                                               
013450     End-if                                                               
013500                                                                          
013600     Goback                                                               
013700     .                                                                    
