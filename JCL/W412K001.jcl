//W412K001 JOB (650W4120100W412K001,W100),'RTN W412D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//IFWX    EXEC VVIDIFRX,ROUTINE=W412D1,MGMTCLAS=BACKUPC,                        
//             DSOUT='W412.W412D1.W41203(+1)',                                  
//             RF=VB,RL=164,BZ=6233,                                            
//             WWD=XXX                                            ?????         
//SOP     EXEC WSOPEND,PROCESS=W412K001                                         
