//WONDEMFT JOB (510ROS39000VILM2T),'KJELL ANDRÉ',                               
//             MSGCLASS=H,MSGLEVEL=(2,0),                                       
//             CLASS=N NOTIFY=V063869                                           
/*JOBPARM LINES=5,CARDS=0,FORMS=STD,LINECT=00,ROOM=ARH3                         
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//MSGOUT OUTPUT JESDS=ALL,DEFAULT=YES,                                          
//  ROOM=ARH3,DEPT='9231',ADDRESS=('VOLVO IT AB',                               
//  '405 08','GOTHENBURG')                                                      
//*                                                                             
// EXEC WFTP,TRANSL=STANDARD                                                    
//* VG42.VD.VOLVO.SE  - gammal                                                  
//* dsod01.it.volvo.se                                                          
//OPENCMD DD *                                                                  
 dsod01.it.volvo.se                                                             
 w0ondem prod456                                                                
//FTPCMD  DD *                                                                  
binary                                                                          
sendsite                                                                        
cd /psfglobres/glob240/overlib                                                  
put 'WV2.TEST.PSF(O1WF2101)' O1WF2101                                           
cd /psfglobres/global/fdeflib                                                   
cd /psfglobres/global/pdeflib                                                   
quit                                                                            
//NOCMD DD * -- SÅNT SOM INTE SKA PUTTAS DENNA GÅNG                             
put 'WV2.TEST.PSF(O1WF2101)' O1WF2101                                           
put 'WV2.TEST.PSF(O1WF2101)' O1WF2101                                           
put 'WV2.TEST.PSF(P1WF2101)' P1WF2101                                           
put 'WV2.TEST.PSF(P1W47525)' P1W47525                                           
put 'WV2.TEST.PSF(F1W47525)' F1W47525                                           
put 'WV2.TEST.PSF(O1W47525)' O1W47525                                           
put 'WV2.QASE.PSF(O1W47508)' O1W47508                                           
put 'WV2.QASE.PSF(P1W47508)' P1W47508                                           
put 'WV2.QASE.PSF(F1W47508)' F1W47508                                           
