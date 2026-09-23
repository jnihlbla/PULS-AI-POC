//W116J180 JOB (640W1160100W116J180,W100),'RTN W116E1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W116    EXEC W116P180,                                                        
//        INDIN=W116.&VCOM                                                      
//*                                                                             
//W11680.W11680D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W116J180                                         
