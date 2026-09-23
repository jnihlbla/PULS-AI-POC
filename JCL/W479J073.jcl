//W479J073 JOB (650W4790100W479J073,W100),'RTN W479V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//SORT    EXEC SORT002G                                                         
//W479    EXEC W479P073                                                         
//SOP     EXEC WSOPEND,PROCESS=W479J073                                         
