//WF10J2IE JOB (670WF100100WF10J2IE,W100),'RTN WF10D2',                         
//             CLASS=N,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*            CLASS=V,USER=?,PASSWORD=?  /KOM IHÅG CLASS!!!!                   
//*1WF10    EXEC WF10P020          /TAG BORT *1 VID KÖRNING                     
//*                                                                             
//*1IEXXXXXXXXX                    /TAG BORT //*1 VID KÖRNING                   
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J2IE                                         
