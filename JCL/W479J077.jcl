//W479J077 JOB (650W4790100W479J077,W100),'RTN W479Q1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W479    EXEC W479P077                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W479J077                                         
