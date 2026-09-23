//W121JFTP JOB (640W1210100W121JFTP,W100),'RTN W121PV',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* Kontaktman på Koncernstandard är Lars Breding                               
//*                                                                             
//* P.g.a. Ford inte godkänner DNS som slutar på volvo.se                       
//* körs med rent IP-nummer, tills gjord beställning på ett                     
//* Ford-godkänt namn har erhållits.                                            
//*                                                                             
//* Andra namn på servern har varit:  ftp.vtd.volvo.se                          
//*                                   violin.vtd.volvo.se                       
//*          IP= 131.97.50.101        dpaw1.vtd.volvo.se                        
//*          IP= 131.97.50.96         ftp.vtd.volvo.se   2002-04-24             
//FTP    EXEC WFTP                                                              
//OPENCMD DD *                                                                  
 131.97.50.96                                                                   
 jostart stopp1                                                                 
//FTPCMD  DD *                                                                  
 LOCSI  FWFriendly                                                              
 SENDSITE                                                                       
 BINARY                                                                         
 LOCSITE RECFM=VB LRECL=500 MGMTCLAS=NOBACKUP                                   
 GET ExpAll.txt  'W121.W121PV.W12119(+1)'                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W121JFTP                                         
