//W432Z1SE JOB (650W4320100W432Z1SE,W100),'RTN W432D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* TILL VIDA                                                                   
//*************  VCAS                                                           
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W432.W432D1.W43233(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PULSPAYMENTS                                           
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W432Z1SE                                         
