//W611ARB  JOB (51091925800,HC2N,01,99,0,,,,0),                                 
//          'LINJER ARBETSRAPPORT',                                             
//          MSGCLASS=H,                                                         
//          CLASS=N                                                             
/*ROUTE  XEQ  LOCAL                                                             
/*ROUTE PRINT NJOV1                                                             
//*                                                                             
//*>>>>>>>OBS ANVÄND HELLRE VILMA-FUNKTION W611ARB <<<<<<<<<<<<                 
//*DENNA JCL'EN SKA SUBMITTAS OM LINJERNA PÅ                                    
//*ARBETSRAPPORTERNA FÖRSVUNNIT                                                 
//*                                                                             
//IEBGENER EXEC PGM=IEBGENER                                                    
//SYSIN     DD  DUMMY                                                           
//SYSPRINT  DD  SYSOUT=*                                                        
//*YSUT2    DD  SYSOUT=(A,,F001),DEST=R3130279                                  
//*YSUT2    DD  SYSOUT=(A,,F001),DEST=R3130271                                  
//SYSUT2    DD  SYSOUT=A,DEST=RSE56444 gm2  ej skall använda (SYSUT2)           
//*YSUT2    DD  SYSOUT=A,DEST=RSE56443 tr       ----- """ -----                 
//SYSUT1    DD  *                                                               
^&l0L ^&f1Y ^&f0X ^*c0003a1329B ^*p0141y0000X^*c0P ^*p0141y1058X^*c0P           
^*p0141y1964X^*c0P ^*p0141y2332X^*c0P ^*c0003a0590B ^*p0704y0353X^*c0P          
^*c0003a0708B ^*p0586y0706X^*c0P ^*c0003a0236B ^*p0350y1293X^*c0P               
^*c0003a1117B ^*p0350y1646X^*c0P ^*c2332a0003b ^*p0141y0000X^*c0P               
^*p0586y0000X^*c0P ^*p0704y0000X^*c0P ^*p0822y0000X^*c0P                        
^*p0940y0000X^*c0P ^*p1058y0000X^*c0P ^*p1294y0000X^*c0P                        
^*p1470y0000X^*c0P ^*c1274a0003b ^*p0350y1058X^*c0P                             
^*p0468y1058X^*c0P ^*c1979a0003b ^*p1176y0353X^*c0P ^(s1p14.4v0s3b5T            
^*p0005y0005xVVOLVO ^*p0005y1063xRRECEIVING REPORT                              
^(s0p16.6h7.2v0s0b8T                                                            
^*p0095y0005xCCar Aftersales ^*p0171y0005xSSequence No                          
^*p0171y1063xDDescription ^*p0171y1969xPPartno ^*p0380y1969xSSeq.No             
^*p0498y1969xRReceived ^*p0616y1969xQQty prio                                   
^*p0734y1969xQQty to kit ^*p0852y1969xQQty PRIM                                 
^*p0970y1969x           ^*p1088y1969xQQty kval.err                              
^*p1206y1969xQQty binned ^*p1324y1969xTTime ^*p0380y1651xAAdv.date              
^*p0498y1651xQQty adv. ^*p0616y1651xQQty prio                                   
^*p0734y1651xQQty to kit ^*p0852y1651xQQty PRIM                                 
^*p0970y1651x           ^*p1088y1651x                                           
^*p1324y1651xRRec.date ^*p0380y1298xAAdvice note                                
^*p0380y1063xSSuppl ^*p0498y1063xAAT ^*p0616y1063xRRR W. goods                  
¤*p0734y1063xBBackorder Quantity                                                
^*p1088y1063xQQty control ^*p1206y1063xCControl code                            
^*p1324y1063xPPossible addr ^*p0616y0711xUUnit                                  
^*p0852y0711xPPT ^*p0970y0711xTT.pack                                           
^*p1088y0711xSS danger goods ^*p1206y0711xTT danger goods                       
^*p0852y0358xVVolume ^*p0970y0358xQQ3 ^*p1088y0358xPProcurer                    
^*p1206y0358xCC Orgin ^*p0616y0005xCCarrier ^*p0852y0005xWWeight                
^*p0970y0005xSStorage area ^*p1088y0005xBBuffer area                            
^*p1324y0005xSSuppliers partnumber                                              
^&f1X ^&f10X ^&f1Y ^&f4X ^Z ^E                                                  
