//W611FORB JOB (510W1110100,HC2N,01,99,0,,,,0),                                 
//          'LINJER PÅ FORB RAPP',                                              
//          MSGCLASS=H,                                                         
//          CLASS=N                                                             
/*ROUTE  XEQ  LOCAL                                                             
/*ROUTE PRINT NJORS2                                                            
//*                                                                             
//*DENNA JCL'EN SKA SUBMITTAS OM LINJERNA PÅ                                    
//*FÖRBEHANDLINGSRAPPORTEN FÖRSVUNNIT                                           
//*                                                                             
//IEBGENER EXEC PGM=IEBGENER                                                    
//SYSIN     DD  DUMMY                                                           
//SYSPRINT  DD  SYSOUT=*                                                        
//SYSUT2    DD  SYSOUT=A,DEST=RSE56444                                          
//SYSUT2    DD  SYSOUT=A,DEST=QSE03908                                          
//SYSUT1    DD  *                                                               
¤&l0L ¤&f2Y ¤&f0X ¤*c0003a1329B ¤*p0141y0000X¤*c0P ¤*p0141y2332X¤*c0P           
¤*c0003a0683B ¤*p0141y1058X¤*c0P ¤*p0141y1763X¤*c0P ¤*c0003a0118B               
¤*p0586y0294X¤*c0P ¤*c0003a0236B ¤*p0586y0588X¤*c0P ¤*p0350y1411X¤*c0P          
¤*p0350y2117X¤*c0P ¤*c2332a0003b ¤*p0141y0000X¤*c0P ¤*p0586y0000X¤*c0P          
¤*p0822y0000X¤*c0P ¤*p1470y0000X¤*c0P ¤*c1274a0003b ¤*p0350y1058X¤*c0P          
¤*p0468y1058X¤*c0P ¤*c1058a0003b ¤*p0704y0000X¤*c0P ¤*c0569a0003b               
¤*p0704y1763X¤*c0P ¤(s1p14.4v0s3b5T ¤*p0005y0005xVVOLVO                         
¤*p0005y1063xAFörbehandlingsrapport ¤(s0p14.4v0s3b5T                            
¤*p0890y0005xLFörpackningsmaterial ¤(s0p16.6h7.2v0s0b8T                         
¤*p0095y0005xCCar Aftersales ¤*p0171y0005xPPartinummer                          
¤*p0171y1063xBBenämning ¤*p0171y1768xAArtnr ¤*p0380y1063xBAntal avis            
¤*p0380y1417xBAntal prio ¤*p0380y1771xBAntal till sats                          
¤*p0380y2125xBFörp.kod                                                          
¤*p0498y1063xBVikt i gram ¤*p0498y1417xBVolym cm3 ¤*p0498y1771xBFörp.typ        
¤*p0498y2125xBEnhet ¤*p0616y0005xLQ3-kvant ¤*p0616y0299xLEmballage              
¤*p0616y0593xLAntalskontroll ¤*p0616y1063xLBuffertplats                         
¤*p0616y1768xLL farligt gods ¤*p0734y0005xLLagerplats                           
¤*p0734y0593xLUrsprung ¤*p0734y1768xLAnt Ro                                     
¤*p0970y0005xLLagerplats ¤*p0970y0593xLEmb artnr                                
¤*p0970y1063xLBenämning ¤*p0970y1768xLQ-typ ¤*p0970y2125xLAntal förp            
¤&f1X ¤&f10X ¤&f1Y ¤&f4Y ¤Z ¤E                                                  
