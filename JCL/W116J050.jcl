//W116J050 JOB (640W1160100W116J050,W100),'RTN W116E3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* COUNTRYX2=&COUNTRYX2                                                        
//*                                                                             
//W116    EXEC W116P050                                                         
//*                                                                             
//W11650.W11650D1 DD *                                                          
&COUNTRYX2                                                                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W116J050                                         
