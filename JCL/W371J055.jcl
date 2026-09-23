//W371J055 JOB (650W3710100W371J055,W100),'RTN W371S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*    USER=&IDUSER                                                             
//*    DC=&IDDC                                                                 
//*    DC-REC=&IDDC-REC                                                         
//*                                                                             
//W371     EXEC W371P055                                                        
//W37155.W37155D1 DD *                                                          
&IDUSER                                                                         
&IDDC                                                                           
&IDDC-REC                                                                       
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W371J055                                            
