//WBACKOUT JOB (540VV401100WBACKOUT),'RTN WWBACK',                              
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=1440,NOTIFY=&BACKUSER                               
/*JOBPARM ROOM=PVV2,LINES=9,CARDS=0,FORMS=1800,LINECT=00                        
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//MSGOUT OUTPUT JESDS=ALL,DEFAULT=YES,                                          
//  ROOM=PVV2,DEPT='57051',ADDRESS=('VOLVO CAR AFTERSALES',                     
//  'TORSLANDA','405 08 GOTHENBURG')                                            
//*                                                                             
//BACKOUT  EXEC WG01BACK,                                                       
//             DSIN=WG01.PGMLOG.&BACKPROC(+0),                                  
//             PSB=&BACKPSB                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WBACKOUT                                         
