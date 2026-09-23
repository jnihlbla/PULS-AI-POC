//W611ARB  JOB (510W6110100,HC2N,01,99,0,,,,0),                                 
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
//ICEGENER EXEC PGM=ICEGENER                                                    
//SYSIN     DD  DUMMY                                                           
//SYSPRINT  DD  SYSOUT=*                                                        
//*YSUT2    DD  SYSOUT=A,DEST=RSE61708 GM1  TEST                                
//*YSUT2    DD  SYSOUT=A,DEST=RSE71646 GM1  STJÄRNMÄRK DE KORT DU               
//*YSUT2    DD  SYSOUT=A,DEST=RSE56444 GM1  ej skall använda (SYSUT2)           
//*YSUT2    DD  SYSOUT=A,DEST=RSE56443 tr       ----- """ -----                 
//*YSUT2    DD  SYSOUT=A,DEST=QSE03908          ----- """ -----                 
//*YSUT2    DD  SYSOUT=A,DEST=QSE10126          ----- """ -----                 
//*YSUT2    DD  SYSOUT=A,DEST=QSE10127          ----- """ -----                 
//SYSUT2    DD  SYSOUT=A,DEST=QSE05238                                          
//*YSUT2    DD  SYSOUT=A,DEST=RSE56444          ----- """ -----                 
//SYSUT1    DD  *                                                               
¤&l0L ¤&f1Y ¤&f0X ¤*c0003a1329B ¤*p0141y0000X¤*c0P ¤*p0141y1058X¤*c0P           
¤*p0141y1964X¤*c0P ¤*p0141y2332X¤*c0P ¤*c0003a0590B ¤*p0704y0353X¤*c0P          
¤*c0003a0708B ¤*p0586y0706X¤*c0P ¤*c0003a0236B ¤*p0350y1293X¤*c0P               
¤*c0003a1117B ¤*p0350y1646X¤*c0P ¤*c2332a0003b ¤*p0141y0000X¤*c0P               
¤*p0586y0000X¤*c0P ¤*p0704y0000X¤*c0P ¤*p0822y0000X¤*c0P                        
¤*p0940y0000X¤*c0P ¤*p1058y0000X¤*c0P ¤*p1294y0000X¤*c0P                        
¤*p1470y0000X¤*c0P ¤*c1274a0003b ¤*p0350y1058X¤*c0P                             
¤*p0468y1058X¤*c0P ¤*c1979a0003b ¤*p1176y0353X¤*c0P ¤(s1p14.4v0s3b5T            
¤*p0005y0005xVVOLVO ¤*p0005y1063xAARBETSRAPPORT ¤(s0p16.6h7.2v0s0b8T            
¤*p0095y0005xCCar Parts      ¤*p0171y0005xPPartinummer                          
¤*p0171y1063xBBenämning ¤*p0171y1969xAArtnr ¤*p0380y1969xPPartinr               
¤*p0498y1969xMMottaget ¤*p0616y1969xAAntal prio                                 
¤*p0734y1969xAAntal till sats ¤*p0852y1969xAAntal PRIM                          
¤*p0970y1969xAAntal SEK ¤*p1088y1969xAAntal kvalf                               
¤*p1206y1969xAAntal inlagt ¤*p1324y1969xTTid ¤*p0380y1651xAAvidatum             
¤*p0498y1651xAAntal avis ¤*p0616y1651xAAntal prio                               
¤*p0734y1651xAAntal till sats ¤*p0852y1651xAAntal PRIM                          
¤*p0970y1651xAAntal SEK ¤*p1088y1651xAAntal anm                                 
¤*p1210y1651xFFifo antal veckor                                                 
¤*p1324y1651xMMot.dat ¤*p0380y1298xFFöljesedel                                  
¤*p0380y1063xLLevnr ¤*p0498y1063xRRT ¤*p0616y1063xAAR med gods                  
¤*p0734y1063xRRestorder Antal                                                   
¤*p0850y1063xEErsättningskod                                                    
¤*p0970y1063xSSkickas till                                                      
¤*p1088y1063xAAntals kontroll ¤*p1206y1063xKKontrollkod                         
¤*p1324y1063xMMöjliga adresser ¤*p0616y0711xEEnhet                              
¤*p0852y0711xFFörp.typ ¤*p0970y0711xEEmballage                                  
¤*p1088y0711xLL farligt gods ¤*p1206y0711xTT farligt gods                       
¤*p0852y0358xVVolym ¤*p0970y0358xQQ3-kvant ¤*p1088y0358xAAnskaffare             
¤*p1206y0358xUUrsprung ¤*p0616y0005xLLastbärare ¤*p0852y0005xVVikt              
¤*p0970y0005xLLagerplats ¤*p1088y0005xBBuffertplats                             
¤*p1324y0005xLLeverantörens artikelnummer                                       
¤&f1X ¤&f10X ¤&f1Y ¤&f4X ¤Z ¤E                                                  
