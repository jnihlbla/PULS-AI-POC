//W611FORB JOB (51091925800,HC2N,01,99,0,,,,0),                                 
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
//*      replace the * with an S for the printer that shall have lines          
//SYSUT2    DD  SYSOUT=A,DEST=R3130279     BORN                                 
//*YSUT2    DD  SYSOUT=A,DEST=R3130271     PREV                                 
//SYSUT1    DD  *                                                               
^&l0L ^&f2Y ^&f0X ^*c0003a1329B ^*p0141y0000X^*c0P ^*p0141y2332X^*c0P           
^*c0003a0683B ^*p0141y1058X^*c0P ^*p0141y1763X^*c0P ^*c0003a0118B               
^*p0586y0294X^*c0P ^*c0003a0236B ^*p0586y0588X^*c0P ^*p0350y1411X^*c0P          
^*p0350y2117X^*c0P ^*c2332a0003b ^*p0141y0000X^*c0P ^*p0586y0000X^*c0P          
^*p0822y0000X^*c0P ^*p1470y0000X^*c0P ^*c1274a0003b ^*p0350y1058X^*c0P          
^*p0468y1058X^*c0P ^*c1058a0003b ^*p0704y0000X^*c0P ^*c0569a0003b               
^*p0704y1763X^*c0P ^(s1p14.4v0s3b5T ^*p0005y0005xVVOLVO                         
^*p0005y1063xPPre packing report ^(s0p14.4v0s3b5T                               
^*p0890y0005xPPre packing material ^(s0p16.6h7.2v0s0b8T                         
^*p0095y0005xCCar Aftersales ^*p0171y0005xSSequence No                          
^*p0171y1063xDDescription ^*p0171y1768xPPart.No ^*p0380y1063xQQty adv.          
^*p0380y1417xQQty prio ^*p0380y1771xQQty to kit                                 
^*p0380y2125xPPT                                                                
^*p0498y1063xWWeight gr ^*p0498y1417xVVolume cm3 ^*p0498y1771xPPT               
^*p0498y2125xUUnit ^*p0616y0005xLQ3 ^*p0616y0299xTT.pack                        
^*p0616y0593xQQty control ^*p0616y1063xBBuffer area                             
^*p0616y1768xSS danger goods ^*p0734y0005xSStorage area                         
^*p0734y0593xOOrigin ^*p0734y1768xTT danger goods                               
^*p0970y0005xSStorage area ^*p0970y0593xPPack part.no                           
^*p0970y1063xDDescription ^*p0970y1768xQQ-typ ^*p0970y2125xQQty pack            
^&f1X ^&f10X ^&f1Y ^&f4Y ^Z ^E                                                  
